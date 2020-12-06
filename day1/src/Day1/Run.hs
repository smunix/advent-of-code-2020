{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day1.Run (run, refine) where

import Day1.Import
import RIO.List.Partial (head)
import RIO.Partial (read)
import System.IO (hGetContents, print)

{- https://adventofcode.com/2020/day/1 -}

{-
--- Day 1: Report Repair ---
After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a tropical island. Surely, Christmas will go on without you.

The tropical island has its own currency and is entirely cash-only. The gold coins used there have a little picture of a starfish; the locals just call them stars. None of the currency exchanges seem to have heard of them, but somehow, you'll need to find fifty of these coins by the time you arrive so you can pay the deposit on your room.

To save your vacation, you need to get all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.

Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.

For example, suppose your expense report contained the following:

1721
979
366
299
675
1456
In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.

Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?

Your puzzle answer was 55776.
-}

test1, test2 :: [] Int
test1 =
  [ 1721,
    979,
    366,
    299,
    675,
    1456
  ]
test2 =
  [ 1721,
    979,
    366,
    299,
    675,
    1456
  ]

part1 :: Int -> [] Int -> (Int, Int)
part1 total xs0 = search finished refine' partial & head
  where
    partial = (xs0, (0, 0))
    finished (_, (a, b))
      | a + b == total = Just (a, b)
      | otherwise = Nothing
    refine' ([], (_, _)) = mempty
    refine' (x : xs, (_, _)) = xs <&> \y -> (xs, (x, y))

search ::
  forall partial solution.
  (partial -> Maybe solution) ->
  (partial -> [] partial) ->
  partial ->
  [] solution
search finished' refine' = search'
  where
    search' partial
      | Just sln <- finished' partial = pure sln
      | otherwise = refine' partial <&> search' & join

{-
--- Part Two ---
The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they had left over from a past vacation. They offer you a second one if you can find three numbers in your expense report that meet the same criteria.

Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together produces the answer, 241861950.

In your expense report, what is the product of the three entries that sum to 2020?

Your puzzle answer was 223162626.

-}

part2 :: Int -> [] Int -> (Int, Int, Int)
part2 total xs0 = search finished refine partial & head
  where
    partial = (xs0, (0, 0, 0))
    finished (_, (a, b, c))
      | a + b + c == total = Just (a, b, c)
      | otherwise = Nothing

refine :: ([] Int, (Int, Int, Int)) -> [] ([] Int, (Int, Int, Int))
refine partial =
  if
      | ([], (_, _, _)) <- partial -> mempty
      | ([_, _], (_, _, _)) <- partial -> mempty
      | ([x, y, z], (_, _, _)) <- partial -> pure (mempty, (x, y, z))
      | (x : xs, (_, _, _)) <- partial ->
        do
          y <- xs
          z <- xs
          pure (xs, (x, y, z))

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  logInfo $ fromString $ show $ part1 2020 test1
  logInfo $ fromString $ show $ part1 1041 test2
  -- logInfo $ fromString $ show $ part2 2020 test1
  let refineT = [1, 2, 3]
  logInfo $ fromString $ show $ refine (refineT, (0, 0, 0)) & (refineT,)
  liftIO $ withFile "day1/assets/inputs.txt" ReadMode \h -> do
    contents <- hGetContents h
    contents
      & words
      <&> read @Int
      & const (print ())
