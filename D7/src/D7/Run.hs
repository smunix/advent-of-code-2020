{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module D7.Run
  ( run,
  )
where

import Algebra.Graph.Label (zero)
import qualified Algebra.Graph.Label as Graph
import Algebra.Graph.Labelled ((-<), (>-))
import qualified Algebra.Graph.Labelled as Graph
import qualified Algebra.Graph.ToGraph as Graph
import qualified Control.Monad.Combinators as ParserComb
import D7.Import hiding (words)
import qualified Data.ByteString.Lazy as StringB
import qualified Data.ByteString.Lazy.Char8 as StringC
import qualified Data.Either as Either
import qualified Data.Hashable as Hash
import qualified Data.Hashable.Lifted as Hash
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import GHC.Generics (Generic1)
import RIO.ByteString.Lazy (hGetContents)
import qualified RIO.HashSet as HSet
import qualified RIO.Set as Set
import qualified Text.Megaparsec as MegaP
import qualified Text.Megaparsec.Byte as MegaPC
import qualified Text.Megaparsec.Byte.Lexer as MegaPCL

{- https://adventofcode.com/2020/day/7 -}

{-

-}

newtype Color t where
  Color :: t -> Color t
  deriving stock (Eq, Ord, Show, Generic, Generic1)

instance (Hash.Hashable t) => Hash.Hashable (Color t)

instance Hash.Hashable1 Color

newtype Adj t where
  Adj :: t -> Adj t
  deriving stock (Eq, Ord, Show, Generic, Generic1)

instance (Hash.Hashable t) => Hash.Hashable (Adj t)

instance Hash.Hashable1 Adj

data From t where
  From ::
    { fromAdj :: Adj t,
      fromColor :: Color t
    } ->
    From t
  deriving stock (Eq, Ord, Show, Generic, Generic1)

instance (Hash.Hashable t) => Hash.Hashable (From t)

instance Hash.Hashable1 From

data To t where
  To ::
    { toCount :: Int64,
      toAdj :: Adj t,
      toColor :: Color t
    } ->
    To t
  deriving stock (Eq, Ord, Show)

-- | The rule representation
data Rule c t where
  Rule ::
    { ruleFrom :: From t,
      ruleTo :: Maybe (c (To t))
    } ->
    Rule c t

data Vertex t where
  NoVertex :: Vertex t
  Vertex ::
    { vertexAdj :: Adj t,
      vertexCol :: Color t
    } ->
    Vertex t
  deriving stock (Eq, Ord, Show, Generic, Generic1)

class ToVertex f where
  toVertex :: f t -> Vertex t

instance ToVertex From where
  toVertex From {..} = Vertex {vertexAdj = fromAdj, vertexCol = fromColor, ..}

instance ToVertex To where
  toVertex To {..} = Vertex {vertexAdj = toAdj, vertexCol = toColor, ..}

deriving instance (Show t, Show (c (To t))) => Show (Rule c t)

-- |
--  The Set of all bags represented as a 'Graph.Optimum' graph
type Bags = Graph.Graph (Graph.Optimum (Graph.Distance Int64) (Graph.Count Int64)) (Vertex StringC.ByteString)

run :: RIO App ()
run = do
  testFile "D7/assets/test.txt"
  testFile "D7/assets/test2.txt"
  testFile "D7/assets/input.txt"
  where
    testFile :: FilePath -> RIO App ()
    testFile fp =
      withFile
        fp
        ReadMode
        ( hGetContents
            >=> ( StringC.lines
                    >>> fmap
                      ( MegaP.runParser
                          ruleParser
                          "rule"
                      )
                    >>> Either.rights
                    >>> ( (fmap toGraph >>> Graph.overlays)
                            &&& rulesFrom
                        )
                    >>> ( ( part1
                              Vertex
                                { vertexAdj = Adj "shiny",
                                  vertexCol = Color "gold"
                                }
                              >>> length
                          )
                            &&& part2
                              Vertex
                                { vertexAdj = Adj "shiny",
                                  vertexCol = Color "gold"
                                }
                        )
                    >>> return
                )
            >=> logInfo . fromString . show
        )

part1 ::
  Vertex StringC.ByteString ->
  ( Bags,
    HSet.HashSet (From StringC.ByteString)
  ) ->
  [] ([] (Vertex StringC.ByteString))
part1 = reaching

part2 ::
  Vertex StringC.ByteString ->
  ( Bags,
    HSet.HashSet (From StringC.ByteString)
  ) ->
  Int64
part2 v0 (graph, _) = go v0 -1 -- do not count init bag
  where
    go :: Vertex StringC.ByteString -> Int64
    go v =
      if
          | List.null neighbors -> 1
          | otherwise ->
            neighbors
              <&> ( \v' ->
                      Graph.edgeLabel v v' graph
                        & Graph.getArgument
                        & Graph.getCount
                        & Graph.getFinite
                        & Maybe.fromMaybe 0
                        & (* go v')
                  )
                & sum
                & (+ 1) -- count parent bag
      where
        neighbors :: [] (Vertex StringC.ByteString)
        neighbors =
          graph
            & Graph.postSet v
            & Set.toList

reaching ::
  Vertex StringC.ByteString ->
  ( Bags,
    HSet.HashSet (From StringC.ByteString)
  ) ->
  [] ([] (Vertex StringC.ByteString))
reaching NoVertex {} (_, _) = mempty
reaching v@Vertex {} (graph, nodes) =
  HSet.toList nodes
    & List.filter (toVertex >>> (/= v))
    & fmap (toVertex >>> (`Graph.reachable` graph))
    & List.filter (List.elem v)

rulesFrom :: [Rule [] StringC.ByteString] -> HSet.HashSet (From StringC.ByteString)
rulesFrom = fmap (ruleFrom >>> HSet.singleton) >>> HSet.unions

-- | Convert a given 'Rule' into a simple 'Graph.Network'
toGraph :: Rule [] StringC.ByteString -> Bags
toGraph Rule {..} =
  if
      | Nothing <- ruleTo -> toVertex ruleFrom -< zero >- NoVertex
      | Just [] <- ruleTo -> toVertex ruleFrom -< zero >- NoVertex
      | Just xs <- ruleTo ->
        xs
          <&> (\t@To {..} -> toVertex ruleFrom -< Graph.Optimum 1 (fromIntegral toCount) >- toVertex t)
          & Graph.overlays

ruleParser :: MegaP.Parsec () StringC.ByteString (Rule [] StringC.ByteString)
ruleParser = do
  ruleFrom <- ruleFromParser
  _ <- MegaPC.space1 *> "bags contain" <* MegaPC.space1
  ruleTo <- ruleToParser
  return Rule {..}

ruleToParser :: MegaP.Parsec () StringC.ByteString (Maybe [To StringC.ByteString])
ruleToParser = (MegaP.try noBagParser $> Nothing) <|> (ParserComb.some toParser <&> Just)

toParser :: MegaP.Parsec () StringC.ByteString (To StringC.ByteString)
toParser = do
  MegaPC.space
  toCount <- MegaPCL.decimal
  MegaPC.space1
  toAdj <- adjParser
  MegaPC.space1
  toColor <- colorParser
  MegaPC.space1
  _ <- MegaP.try "bags" <|> "bag"
  _ <- MegaP.try "." <|> ","
  return To {..}

noBagParser :: MegaP.Parsec () StringC.ByteString StringC.ByteString
noBagParser = "no other bags"

ruleFromParser :: MegaP.Parsec () StringC.ByteString (From StringC.ByteString)
ruleFromParser = do
  fromAdj <- adjParser
  MegaPC.space1
  fromColor <- colorParser
  return From {..}

colorParser :: MegaP.Parsec () StringC.ByteString (Color StringC.ByteString)
colorParser = ParserComb.many MegaPC.letterChar <&> (StringB.pack >>> Color)

adjParser :: MegaP.Parsec () StringC.ByteString (Adj StringC.ByteString)
adjParser = ParserComb.many MegaPC.letterChar <&> (StringB.pack >>> Adj)
