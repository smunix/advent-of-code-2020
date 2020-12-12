{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
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

module D4.Run
  ( run,
  )
where

import qualified Control.Monad.Combinators as ParserComb
import D4.Import hiding (words)
import qualified Data.ByteString.Lazy as ByteStringW
import qualified Data.ByteString.Lazy.Char8 as ByteStringC
import Data.Default
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Strings as Strings
import qualified Debug.Trace as Debug
import qualified GHC.TypeLits as Ghc
import RIO.ByteString.Lazy (hGetContents)
import qualified Text.Megaparsec as MegaP
import qualified Text.Megaparsec.Byte as MegaPB
import qualified Text.Megaparsec.Byte.Lexer as MegaPBL

{- https://adventofcode.com/2020/day/4 -}

{-

--- Day 4: Passport Processing ---
You arrive at the airport only to realize that you grabbed your North Pole
Credentials instead of your passport. While these documents are extremely
similar, North Pole Credentials aren't issued by a country and therefore aren't
actually valid documentation for travel in most of the world.

It seems like you're not the only one having problems, though; a very long line
has formed for the automatic passport scanners, and the delay could upset your
travel itinerary.

Due to some questionable network security, you realize you might be able to
solve both of these problems at the same time.

The automatic passport scanners are slow because they're having trouble
detecting which passports have all required fields. The expected fields are as
follows:

byr (Birth Year)
iyr (Issue Year)
eyr (Expiration Year)
hgt (Height)
hcl (Hair Color)
ecl (Eye Color)
pid (Passport ID)
cid (Country ID)

Passport data is validated in batch files (your puzzle input). Each passport is
represented as a sequence of key:value pairs separated by spaces or newlines.
Passports are separated by blank lines.

Here is an example batch file containing four passports:

ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in

The first passport is valid - all eight fields are present. The second passport
is invalid - it is missing hgt (the Height field).

The third passport is interesting; the only missing field is cid, so it looks
like data from North Pole Credentials, not a passport at all! Surely, nobody
would mind if you made the system temporarily ignore missing cid fields. Treat
this "passport" as valid.

The fourth passport is missing two fields, cid and byr. Missing cid is fine, but
missing any other field is not, so this passport is invalid.

According to the above rules, your improved system would report 2 valid
passports.

Count the number of valid passports - those that have all required fields. Treat
cid as optional. In your batch file, how many passports are valid?

To begin, get your puzzle input.

Answer: 226

-}

-- |
--  Represents the Passport specification.
--  The context 'm' tells when a field is missing or present.
--  Examples of contexts we can use are: 'Maybe', '[]'
data Passport m where
  -- | Passport constructor
  Passport ::
    { -- | Birth Year
      byr :: m Field,
      -- | Issue Year
      iyr :: m Field,
      -- | Expiration Year
      eyr :: m Field,
      -- | Height
      hgt :: m Field,
      -- | Hair Color
      hcl :: m Field,
      -- | Eye Color
      ecl :: m Field,
      -- | Passpord ID
      pid :: m Field,
      -- | Country ID
      cid :: m Field
    } ->
    Passport m

class IsValid1 m where
  isValid1 :: Passport m -> Bool

instance IsValid1 Maybe where
  isValid1 Passport {..} =
    if
        | (Just _, Just _, Just _, Just _, Just _, Just _, Just _, _) <- (byr, iyr, eyr, hgt, hcl, ecl, pid, cid) -> True
        | otherwise -> False

class (IsValid1 m) => IsValid2 m where
  isValid2 :: Passport m -> Bool

instance IsValid2 Maybe where
  isValid2 Passport {..} =
    if
        | ( Just (Byr byr'),
            Just (Iyr iyr'),
            Just (Eyr eyr'),
            Just (Hgt hgt'),
            Just (Hcl hcl'),
            Just (Ecl ecl'),
            Just (Pid pid'),
            _
            ) <-
            ( byr,
              iyr,
              eyr,
              hgt,
              hcl,
              ecl,
              pid,
              cid
            ) ->
          and
            [ interval (1920, 2002) byr',
              interval (2010, 2020) iyr',
              interval (2020, 2030) eyr',
              hgt' & validHeight,
              hcl' & validHairColor,
              ecl' & validEyeColor,
              pid' & validPid
            ]
        | otherwise -> False

validPid :: LByteString -> Bool
validPid = fromRight False . MegaP.runParser parser "pid"
  where
    parser :: MegaP.Parsec () LByteString Bool
    parser = ParserComb.count 9 MegaPB.digitChar $> True

validEyeColor :: LByteString -> Bool
validEyeColor = fromRight False . MegaP.runParser parser "ecl"
  where
    parser :: MegaP.Parsec () LByteString Bool
    parser =
      ParserComb.choice
        [ "amb",
          "blu",
          "brn",
          "gry",
          "grn",
          "hzl",
          "oth"
        ]
        $> True

validHairColor :: LByteString -> Bool
validHairColor = fromRight False . MegaP.runParser parser "hcl"
  where
    parser :: MegaP.Parsec () LByteString Bool
    parser =
      "#"
        *> ParserComb.count 6 MegaPB.hexDigitChar
        $> True

validHeight :: LByteString -> Bool
validHeight = fromRight False . MegaP.runParser parser "hgt"
  where
    parser :: MegaP.Parsec () LByteString Bool
    parser =
      (MegaPBL.decimal :: MegaP.Parsec () LByteString Int64)
        <&> ( \v ->
                ParserComb.choice ["cm", "in"]
                  <&> \case
                    "cm" -> 150 <= v && v <= 193
                    "in" -> 59 <= v && v <= 76
                    _ -> False
            )
        & join

interval ::
  (Int64, Int64) ->
  LByteString ->
  Bool
interval (l, h) = fromRight False . MegaP.runParser parser "int"
  where
    parser :: MegaP.Parsec () LByteString Bool
    parser =
      ParserComb.count 4 MegaPB.digitChar
        <&> MegaP.runParser (MegaPBL.decimal :: MegaP.Parsec () [Word8] Int64) "ints"
        <&> either (const False) (uncurry (&&) . ((l <=) &&& (<= h)))

deriving stock instance (Show (m Field)) => Show (Passport m)

instance (Semigroup (m Field)) => Semigroup (Passport m) where
  a <> b =
    Passport
      { byr = (byr *** byr) (a, b) & uncurry (<>),
        iyr = (iyr *** iyr) (a, b) & uncurry (<>),
        eyr = (eyr *** eyr) (a, b) & uncurry (<>),
        hgt = (hgt *** hgt) (a, b) & uncurry (<>),
        hcl = (hcl *** hcl) (a, b) & uncurry (<>),
        ecl = (ecl *** ecl) (a, b) & uncurry (<>),
        pid = (pid *** pid) (a, b) & uncurry (<>),
        cid = (cid *** cid) (a, b) & uncurry (<>),
        ..
      }

instance (Monoid (m Field)) => Monoid (Passport m) where
  mempty =
    Passport
      { byr = mempty,
        iyr = mempty,
        eyr = mempty,
        hgt = mempty,
        hcl = mempty,
        ecl = mempty,
        pid = mempty,
        cid = mempty
      }

instance Monoid (m Field) => Default (Passport m) where
  def = mempty

hoist :: Passport [] -> Passport Maybe
hoist p =
  Passport
    { byr = Maybe.listToMaybe $ byr p,
      iyr = Maybe.listToMaybe $ iyr p,
      eyr = Maybe.listToMaybe $ eyr p,
      hgt = Maybe.listToMaybe $ hgt p,
      hcl = Maybe.listToMaybe $ hcl p,
      ecl = Maybe.listToMaybe $ ecl p,
      pid = Maybe.listToMaybe $ pid p,
      cid = Maybe.listToMaybe $ cid p,
      ..
    }

{-
Your puzzle answer was 226.

The first half of this puzzle is complete! It provides one gold star: *

--- Part Two --- The line is moving more quickly now, but you overhear airport
security talking about how passports with invalid data are getting through.
Better add some data validation, quick!

You can continue to ignore the cid field, but each other field has strict rules
about what values are valid for automatic validation:

byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.

Your job is to count the passports where all required fields are both present
and valid according to the above rules. Here are some example values:

byr valid:   2002
byr invalid: 2003

hgt valid:   60in
hgt valid:   190cm
hgt invalid: 190in
hgt invalid: 190

hcl valid:   #123abc
hcl invalid: #123abz
hcl invalid: 123abc

ecl valid:   brn
ecl invalid: wat

pid valid:   000000001
pid invalid: 0123456789
Here are some invalid passports:

eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
Here are some valid passports:

pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719

Count the number of valid passports - those that have all required fields and
valid values. Continue to treat cid as optional. In your batch file, how many
passports are valid?

Answer:
-}

-- |
-- 'Passport' field descriptor
data Field where
  -- | Birth Year
  Byr :: LByteString -> Field
  -- | Issue Year
  Iyr :: LByteString -> Field
  -- | Expiry Year
  Eyr :: LByteString -> Field
  -- | Height
  Hgt :: LByteString -> Field
  -- | Hair Color
  Hcl :: LByteString -> Field
  -- | Eyes Color
  Ecl :: LByteString -> Field
  -- | Passport ID
  Pid :: LByteString -> Field
  -- | Country ID
  Cid :: LByteString -> Field
  deriving stock (Show)

fieldP :: MegaP.ParsecT () LByteString m Field
fieldP =
  ParserComb.choice
    [ tag @"byr" MegaPB.digitChar,
      tag @"iyr" MegaPB.digitChar,
      tag @"eyr" MegaPB.digitChar,
      tag @"hgt" MegaPB.asciiChar,
      tag @"hcl" MegaPB.asciiChar,
      tag @"ecl" MegaPB.asciiChar,
      tag @"pid" MegaPB.digitChar,
      tag @"cid" MegaPB.digitChar
    ]

class (Ghc.KnownSymbol s) => Tag s where
  tagCtor :: LByteString -> Field

  tagTxt :: LByteString
  tagTxt = Ghc.symbolVal (Proxy @s) & ByteStringC.pack

  tag :: MegaP.ParsecT () LByteString m Word8 -> MegaP.ParsecT () LByteString m Field
  tag c = MegaPB.string (ByteStringC.append (tagTxt @s) ":") >> (many c <* MegaPB.space) <&> (tagCtor @s) . ByteStringW.pack

instance Tag "byr" where tagCtor = Byr

instance Tag "iyr" where tagCtor = Iyr

instance Tag "eyr" where tagCtor = Eyr

instance Tag "hgt" where tagCtor = Hgt

instance Tag "hcl" where tagCtor = Hcl

instance Tag "ecl" where tagCtor = Ecl

instance Tag "pid" where tagCtor = Pid

instance Tag "cid" where tagCtor = Cid

fieldsP :: MegaP.ParsecT () LByteString m ([] Field)
fieldsP = many fieldP

passportP ::
  ( Applicative m',
    Monoid (m' Field)
  ) =>
  MegaP.ParsecT () LByteString m (Passport m')
passportP =
  fieldsP
    >>= foldMap
      ( \case
          a@Byr {} -> pure def {byr = pure a}
          a@Iyr {} -> pure def {iyr = pure a}
          a@Eyr {} -> pure def {eyr = pure a}
          a@Hgt {} -> pure def {hgt = pure a}
          a@Hcl {} -> pure def {hcl = pure a}
          a@Ecl {} -> pure def {ecl = pure a}
          a@Pid {} -> pure def {pid = pure a}
          a@Cid {} -> pure def {cid = pure a}
      )

-- | produce passports from a currated list of 'LByteString'
passports ::
  forall m' m.
  ( Monoid (m' Field),
    Monad m,
    Applicative m'
  ) =>
  [] ([] LByteString) ->
  m ([] (Passport m'))
passports (List.null -> True) = return mempty
passports xs = do
  xs
    <&> ( \ps ->
            ps
              <&> ( MegaP.runParserT (passportP @m') fp
                      >=> return . either (const mempty) id
                  )
              & sequence
              <&> fold
        )
    & sequence
  where
    fp = "D4/assets/test.txt"

run :: RIO App ()
run = do
  testFile "D4/assets/test-invalid-2.txt"
  testFile "D4/assets/test-valid-2.txt"
  testFile "D4/assets/input.txt"
  where
    testFile :: FilePath -> RIO App ()
    testFile fp = withFile fp ReadMode \fd -> do
      contents <- hGetContents fd
      let lines' :: [] ([] LByteString)
          lines' =
            contents
              & ByteStringC.lines
              & List.groupBy onePassport
              & List.filter (/= [""])
              <&> join . (<&> ByteStringC.words)

          allPassports :: RIO App ([] (Passport Maybe))
          allPassports = passports @[] @(RIO App) lines' <&> fmap hoist

      -- lines'
      --   & logInfo . fromString . show

      -- allPassports
      --   >>= logInfo . fromString . show

      allPassports
        <&> length . filter (== True) . fmap isValid1 &&& length . filter (== True) . fmap (\x -> if isValid2 x then Debug.traceShow (x, True) True else False)
        >>= \r -> do
          logInfo $ fromString fp
          logInfo $ fromString $ Strings.sToString contents
          logInfo $ fromString $ show (fp, r)

    onePassport :: LByteString -> LByteString -> Bool
    onePassport a b =
      if
          | (False, True) <- (a', b') -> False
          | (True, False) <- (a', b') -> False
          | otherwise -> True
      where
        a', b' :: Bool
        (!a', !b') = (ByteStringC.null *** ByteStringC.null) (a, b)
