{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module D2.Run
  ( run,
  )
where

import D2.Import hiding (words)
import Data.ByteString.Lazy (index, pack)
import RIO.ByteString.Lazy (count, hGetContents)
import RIO.Char (ord)
import Text.Megaparsec (ParseErrorBundle)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P
import qualified Text.Megaparsec.Byte.Lexer as P

{- https://adventofcode.com/2020/day/2 -}

{-
--- Day 2: Password Philosophy ---
Your flight departs in a few days from the coastal airport; the easiest way down to the coast from here is via toboggan.

The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day. "Something's wrong with our computers; we can't log in!" You ask if you can take a look.

Their password database seems to be a little corrupted: some of the passwords wouldn't have been allowed by the Official Toboggan Corporate Policy that was in effect when they were chosen.

To try to debug the problem, they have created a list (your puzzle input) of passwords (according to the corrupted database) and the corporate policy when that password was set.

For example, suppose you have the following list:

1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.

In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1. The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies.

How many passwords are valid according to their policies?

To begin, get your puzzle input.

"D2/assets/part1.txt"

Answer: 622
-}

data Entry where
  Entry ::
    { low :: Int64,
      high :: Int64,
      char :: Word8,
      password :: LByteString
    } ->
    Entry
  deriving stock (Show)

entriesParser :: P.ParsecT () LByteString (RIO App) ([] Entry)
entriesParser = some (entryParser <* P.newline)
  where
    entryParser :: P.ParsecT () LByteString (RIO App) Entry
    entryParser = do
      low <- P.decimal
      _ <- P.char (fromIntegral $ ord '-')
      high <- P.decimal
      _ <- P.anySingle
      char <- P.alphaNumChar
      _ <- P.string ": "
      password <- some P.alphaNumChar <&> pack
      pure Entry {..}

solve1 :: [] Entry -> RIO App Int
solve1 entries = do
  logInfo . fromString . show $ entries
  entries
    <&> isValid
    & catMaybes
    & length
    & pure
  where
    isValid :: Entry -> Maybe Entry
    isValid e@Entry {..} =
      if low <= c && c <= high
        then Just e
        else Nothing
      where
        c = count char password & fromIntegral

{-
our puzzle answer was 622.

The first half of this puzzle is complete! It provides one gold star: *

--- Part Two ---
While it appears you validated the passwords correctly, they don't seem to be what the Official Toboggan Corporate Authentication System is expecting.

The shopkeeper suddenly realizes that he just accidentally explained the password policy rules from his old job at the sled rental place down the street! The Official Toboggan Corporate Policy actually works a little differently.

Each policy actually describes two positions in the password, where 1 means the first character, 2 means the second character, and so on. (Be careful; Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of these positions must contain the given letter. Other occurrences of the letter are irrelevant for the purposes of policy enforcement.

Given the same example list from above:

1-3 a: abcde is valid: position 1 contains a and position 3 does not.
1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
How many passwords are valid according to the new interpretation of the policies?

Answer: 263

Although it hasn't changed, you can still get your puzzle input.

-}

solve2 :: [] Entry -> RIO App Int
solve2 entries = do
  logInfo . fromString . show $ entries
  entries
    <&> isValid
    & catMaybes
    & length
    & pure
  where
    isValid :: Entry -> Maybe Entry
    isValid e@Entry {..} = case (check low, check high) of
      (True, False) -> pure e
      (False, True) -> pure e
      _ -> Nothing
      where
        check = (char ==) . index password . flip (-) 1

run :: RIO App ()
run = do
  withFile fp ReadMode \h -> do
    contents <- hGetContents h
    P.runParserT entriesParser fp contents
      <&> either (\x -> (,) <$> failFn x <*> failFn x) (\x -> (,) <$> solve1 x <*> solve2 x)
      & join
      >>= (logInfo . fromString . show)
  where
    fp = "D2/assets/part1.txt"
    failFn :: ParseErrorBundle LByteString () -> RIO App Int
    failFn = logError . fromString . show >=> const (pure 0)
