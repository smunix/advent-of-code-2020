{-# LANGUAGE BangPatterns #-}
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

module D3.Run
  ( run,
  )
where

import D3.Import hiding (words)
import Data.Array (Array)
import qualified Data.Array as Array
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Maybe as Maybe
import RIO.ByteString.Lazy (hGetContents)

{- https://adventofcode.com/2020/day/3 -}

{-
--- Day 3: Toboggan Trajectory --- With the toboggan login problems resolved,
you set off toward the airport. While travel by toboggan might be easy, it's
certainly not safe: there's very minimal steering and the area is covered in
trees. You'll need to see which angles will take you near the fewest trees.

Due to the local geology, trees in this area only grow on exact integer
coordinates in a grid. You make a map (your puzzle input) of the open squares
(.) and trees (#) you can see. For example:

..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
These aren't the only trees, though; due to something you read about once
involving arboreal genetics and biome stability, the same pattern repeats to the
right many times:

..##.........##.........##.........##.........##.........##.......  --->
#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
.#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
.#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
.#........#.#........#.#........#.#........#.#........#.#........#
#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
#...##....##...##....##...##....##...##....##...##....##...##....#
.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
You start on the open square (.) in the top-left corner and need to reach the
bottom (below the bottom-most row on your map).

The toboggan can only follow a few specific slopes (you opted for a cheaper
model that prefers rational numbers); start by counting all the trees you would
encounter for the slope right 3, down 1:

From your starting position at the top-left, check the position that is right 3
and down 1. Then, check the position that is right 3 and down 1 from there, and
so on until you go past the bottom of the map.

The locations you'd check in the above example are marked here with O where
there was an open square and X where there was a tree:

..##.........##.........##.........##.........##.........##.......  --->
#..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
.#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
.#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
.#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
.#........#.#........X.#........#.#........#.#........#.#........#
#.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
#...##....##...##....##...#X....##...##....##...##....##...##....#
.#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
In this example, traversing the map using this slope would cause you to
encounter 7 trees.

Starting at the top-left corner of your map and following a slope of right 3 and
down 1, how many trees would you encounter?

To begin, get your puzzle input.

Answer: 216
-}

treeChar, spaceChar :: Char
(treeChar, spaceChar) = ('#', '.')

startPos :: (Int64, Int64)
startPos = (1, 1)

data Step where
  Step ::
    { stepCount :: !Int,
      stepPos :: !(Int64, Int64)
    } ->
    Step
  deriving stock (Show)

slopeFn ::
  -- | our grid
  Array (Int64, Int64) Char ->
  -- | slope
  (Int64, Int64) ->
  -- | initial position
  Step ->
  Maybe Step
slopeFn arr slope Step {..} =
  if
      | height < fst stepPos -> Nothing
      | treeChar == arr Array.! stepPos ->
        Just
          Step
            { stepCount = stepCount + 1,
              stepPos = add stepPos slope
            }
      | spaceChar == arr Array.! stepPos ->
        Just
          Step
            { stepPos = add stepPos slope,
              ..
            }
      | otherwise -> Nothing
  where
    height, width :: Int64
    (_, (height, width)) = Array.bounds arr

    add :: (Int64, Int64) -> (Int64, Int64) -> (Int64, Int64)
    add (x, y) (x', y') = (xx, yy)
      where
        xx, yy :: Int64
        !xx = x + x'
        !yy' = y + y'
        !yy
          | width < yy' = yy' `mod` width
          | otherwise = yy'

{-

In this example, traversing the map using this slope would cause you to
encounter 7 trees.

Starting at the top-left corner of your map and following a slope of right 3 and
down 1, how many trees would you encounter?

Your puzzle answer was 216.

The first half of this puzzle is complete! It provides one gold star: *

--- Part Two ---

Time to check the rest of the slopes - you need to minimize the
probability of a sudden arboreal stop, after all.

Determine the number of trees you would encounter if, for each of the following
slopes, you start at the top-left corner and traverse the map all the way to the
bottom:

Right 1, down 1.
Right 3, down 1. (This is the slope you already checked.)
Right 5, down 1.
Right 7, down 1.
Right 1, down 2.
In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s)
respectively; multiplied together, these produce the answer 336.

What do you get if you multiply together the number of trees encountered on each
of the listed slopes?

Answer: 6708199680

-}

run :: RIO App ()
run = do
  withFile fp ReadMode \h -> do
    contents <- hGetContents h
    let lines' :: [] ByteString.ByteString
        lines' = contents & ByteString.lines

        size ::
          -- | (height, width)
          Maybe (Int64, Int64)
        size = (,) <$> ht lines' <*> wt lines'

        ht :: [] ByteString.ByteString -> Maybe Int64
        ht = pure . fromIntegral . length

        wt :: [] ByteString.ByteString -> Maybe Int64
        wt = (ByteString.length <$>) . Maybe.listToMaybe

        grid :: Maybe (Array (Int64, Int64) Char)
        grid = size <&> flip Array.listArray (lines' <&> ByteString.unpack & join) . (startPos,)

    let part1 = solve . flip slopeFn (1, 3)

        part2 = \arr ->
          [ (1, 1),
            (1, 3),
            (1, 5),
            (1, 7),
            (2, 1)
          ]
            <&> (solve . slopeFn arr)
            & sequence
            <&> Maybe.catMaybes
            <&> Just . product

    grid
      & maybe (return (Nothing, Nothing)) (\arr -> (,) <$> part1 arr <*> part2 arr)
      >>= logInfo . fromString . show
  where
    -- fp = "D3/assets/test1.txt"
    fp = "D3/assets/part1.txt"

solve ::
  (Step -> Maybe Step) ->
  RIO App (Maybe Int)
solve stepFn =
  loop
    False
    Step
      { stepCount = 0,
        stepPos = startPos
      }
  where
    loop ::
      Bool ->
      Step ->
      RIO App (Maybe Int)
    loop b s@Step {..} =
      if
          | (False, Nothing) <- (b, stepFn s) -> return Nothing
          | (_, Just s') <- (b, stepFn s) -> loop True s'
          | otherwise -> return $ Just stepCount
