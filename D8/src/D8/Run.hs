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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module D8.Run
  ( run,
    solve,
  )
where

import D8.Import hiding (words)
import qualified Data.Array as Array
import qualified Data.ByteString.Lazy.Char8 as StringC
import qualified Data.Maybe as Maybe
import qualified Debug.Trace as Debug
import qualified RIO.HashSet as HSet
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as PB
import qualified Text.Megaparsec.Byte.Lexer as PBL

{- https://adventofcode.com/2020/day/8 -}

{-

--- Day 8: Handheld Halting ---
Your flight to the major airline hub reaches cruising altitude without incident.
While you consider checking the in-flight menu for one of those drinks that come
with a little umbrella, you are interrupted by the kid sitting next to you.

Their handheld game console won't turn on! They ask if you can take a look.

You narrow the problem down to a strange infinite loop in the boot code (your
puzzle input) of the device. You should be able to fix it, but first you need to
be able to run the code in isolation.

The boot code is represented as a text file with one instruction per line of
text. Each instruction consists of an operation (acc, jmp, or nop) and an
argument (a signed number like +4 or -20).

acc increases or decreases a single global value called the accumulator by the
value given in the argument. For example, acc +7 would increase the accumulator
by 7. The accumulator starts at 0. After an acc instruction, the instruction
immediately below it is executed next. jmp jumps to a new instruction relative
to itself. The next instruction to execute is found using the argument as an
offset from the jmp instruction; for example, jmp +2 would skip the next
instruction, jmp +1 would continue to the instruction immediately below it, and
jmp -20 would cause the instruction 20 lines above to be executed next. nop
stands for No OPeration - it does nothing. The instruction immediately below it
is executed next.
For example, consider the following program:

nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
These instructions are visited in this order:

nop +0  | 1
acc +1  | 2, 8(!)
jmp +4  | 3
acc +3  | 6
jmp -3  | 7
acc -99 |
acc +1  | 4
jmp -4  | 5
acc +6  |
First, the nop +0 does nothing. Then, the accumulator is increased from 0 to 1
(acc +1) and jmp +4 sets the next instruction to the other acc +1 near the
bottom. After it increases the accumulator from 1 to 2, jmp -4 executes, setting
the next instruction to the only acc +3. It sets the accumulator to 5, and jmp
-3 causes the program to continue back at the first acc +1.

This is an infinite loop: with this sequence of jumps, the program will run
forever. The moment the program tries to run any instruction a second time, you
know it will never terminate.

Immediately before the program would run an instruction a second time, the value
in the accumulator is 5.

Run your copy of the boot code. Immediately before any instruction is executed a
second time, what value is in the accumulator?

Your puzzle answer was 1337.

--- Part Two ---
After some careful analysis, you believe that exactly one instruction is
corrupted.

Somewhere in the program, either a jmp is supposed to be a nop, or a nop is
supposed to be a jmp. (No acc instructions were harmed in the corruption of this
boot code.)

The program is supposed to terminate by attempting to execute an instruction
immediately after the last instruction in the file. By changing exactly one jmp
or nop, you can repair the boot code and make it terminate correctly.

For example, consider the same program from above:

nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
If you change the first instruction from nop +0 to jmp +0, it would create a
single-instruction infinite loop, never leaving that instruction. If you change
almost any of the jmp instructions, the program will still eventually find
another jmp instruction and loop forever.

However, if you change the second-to-last instruction (from jmp -4 to nop -4),
the program terminates! The instructions are visited in this order:

nop +0  | 1
acc +1  | 2
jmp +4  | 3
acc +3  |
jmp -3  |
acc -99 |
acc +1  | 4
nop -4  | 5
acc +6  | 6
After the last instruction (acc +6), the program terminates by attempting to run
the instruction below the last instruction in the file. With this change, after
the program terminates, the accumulator contains the value 8 (acc +1, acc +1,
acc +6).

Fix the program so that it terminates normally by changing exactly one jmp (to
nop) or nop (to jmp). What is the value of the accumulator after the program
terminates?

Your puzzle answer was 1358.

Both parts of this puzzle are complete! They provide two gold stars: **

-}

data Instr where
  Noop :: Int -> Instr
  Jmp :: Int -> Instr
  Acc :: Int -> Instr
  deriving stock (Show, Eq, Ord, Generic)

data Console where
  Console ::
    { consMemory :: Array.Array Int Instr,
      consPointer :: Int,
      consAccumulator :: Int,
      consSeen :: HSet.HashSet Int,
      consSteps :: [] (Int, Instr, Int)
    } ->
    Console
  deriving stock (Show, Generic)

dbg :: Show b => b -> b
dbg x = Debug.traceShow x x

part1 ::
  Console ->
  Maybe Console
part1 = runBoot
  where
    step :: Console -> Maybe Console
    step Console {..} =
      if
          | consPointer `HSet.member` consSeen -> Nothing
          | Jmp i <- currInstr ->
            return
              Console
                { consPointer = consPointer + i,
                  consSeen = HSet.insert consPointer consSeen,
                  ..
                }
          | Acc i <- currInstr ->
            return
              Console
                { consPointer = consPointer + 1,
                  consAccumulator = consAccumulator + i,
                  consSeen = HSet.insert consPointer consSeen,
                  ..
                }
          | otherwise ->
            return
              Console
                { consPointer = consPointer + 1,
                  consSeen = HSet.insert consPointer consSeen,
                  ..
                }
      where
        currInstr :: Instr
        currInstr = consMemory Array.! consPointer

    runBoot :: Console -> Maybe Console
    runBoot c =
      step c
        & ( \case
              Just c' -> runBoot c'
              Nothing -> Just c
          )

part2 ::
  Console ->
  Maybe Console
part2 = runBoot
  where
    step :: Console -> Maybe (Either Console Console)
    step c@Console {..} =
      if
          | consPointer `HSet.member` consSeen -> Nothing
          | maxPointer < consPointer -> return . Right $ c
          -- | maxPointer == consPointer -> return . Right $ c
          | Jmp i <- currInstr ->
            return $
              Left
                Console
                  { consPointer = consPointer + i,
                    consSeen = HSet.insert consPointer consSeen,
                    consSteps = consSteps <> [(consPointer, currInstr, consAccumulator)],
                    ..
                  }
          | Acc i <- currInstr ->
            return $
              Left
                Console
                  { consPointer = consPointer + 1,
                    consAccumulator = consAccumulator + i,
                    consSeen = HSet.insert consPointer consSeen,
                    consSteps = consSteps <> [(consPointer, currInstr, consAccumulator)],
                    ..
                  }
          | otherwise ->
            return $
              Left
                Console
                  { consPointer = consPointer + 1,
                    consSeen = HSet.insert consPointer consSeen,
                    consSteps = consSteps <> [(consPointer, currInstr, consAccumulator)],
                    ..
                  }
      where
        currInstr :: Instr
        currInstr = consMemory Array.! consPointer
        maxPointer :: Int
        (_, maxPointer) = Array.bounds consMemory

    runBoot :: Console -> Maybe Console
    runBoot c@Console {..} =
      if
          | Noop i <- currInstr -> runBoot'Moded c {consMemory = consMemory Array.// [(consPointer, Jmp i)]} <|> runBoot'Unchanged c
          | Jmp i <- currInstr -> runBoot'Moded c {consMemory = consMemory Array.// [(consPointer, Noop i)]} <|> runBoot'Unchanged c
          | otherwise -> runBoot'Unchanged c
      where
        currInstr :: Instr
        currInstr = consMemory Array.! consPointer

    runBoot'Moded :: Console -> Maybe Console
    runBoot'Moded c =
      step c
        & ( \case
              Just (Left c'@Console {..}) -> runBoot'Moded c'
              Just (Right c'@Console {..}) -> Just c'
              Nothing -> Nothing
          )

    runBoot'Unchanged :: Console -> Maybe Console
    runBoot'Unchanged c =
      step c
        & ( \case
              Just (Left c'@Console {..}) -> runBoot c'
              Just (Right c'@Console {..}) -> Just c'
              Nothing -> Nothing
          )

solve :: StringC.ByteString -> RIO App ()
solve =
  ( StringC.lines
      >>> Maybe.mapMaybe \case
        (StringC.words -> ["nop", i]) -> P.runParser @() (PBL.signed PB.space PBL.decimal <&> return . Noop) "noop" i & fromRight Nothing
        (StringC.words -> ["jmp", i]) -> P.runParser @() (PBL.signed PB.space PBL.decimal <&> return . Jmp) "jmp" i & fromRight Nothing
        (StringC.words -> ["acc", i]) -> P.runParser @() (PBL.signed PB.space PBL.decimal <&> return . Acc) "acc" i & fromRight Nothing
        _ -> Nothing
      >>> ((1,) . length &&& id)
      >>> uncurry Array.listArray
      >>> \arr ->
        return . (fmap consAccumulator *** fmap consAccumulator) . (part1 &&& part2) $
          Console
            { consMemory = arr,
              consPointer = 1,
              consAccumulator = 0,
              consSeen = HSet.empty,
              consSteps = mempty
            }
  )
    >=> logInfo . fromString . show

run :: RIO App ()
run = do
  testFile @(RIO App) "D8/assets/test.txt" solve
  testFile @(RIO App) "D8/assets/input.txt" solve
