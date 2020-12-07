{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day4 where

import qualified Data.Bifunctor
import Data.Text (Text (), breakOn, dropEnd, empty, pack, replace, splitOn, take, takeEnd, unpack, words)
import qualified Data.Text.IO as TextIO
import Prelude

class Parse a where
  parse :: [(Text, Text)] -> Either String a

class Validation a where
  isValid :: a -> Bool

runDay4 :: IO ()
runDay4 = do
  print "--- Day 4: Passport Processing ---"
  print "Part 1: In your batch file, how many passports are valid?"
  result <- readFileContentToLine filepath
  print . length $ filter isPassword result
  print "Part 2: In your batch file, how many passports are valid?"
  print $ countValidPasswords result
  where
    filepath = "resources/day-4-input.txt"

readFileContentToLine :: String -> IO [Text]
readFileContentToLine filePath = fmap (replace newLine (pack " ")) . splitOn emptyNewLine <$> TextIO.readFile filePath
  where
    emptyNewLine = pack "\n\n"
    newLine = pack "\n"

isPassword :: Text -> Bool
isPassword value = null found || found == [pack "cid"]
  where
    allWords = Data.Text.take 3 <$> Data.Text.words value
    found = filter (`notElem` allWords) expectedItems

expectedItems :: [Text]
expectedItems = fmap pack ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

countPasswords :: [Text] -> Int
countPasswords = length . filter isPassword

countValidPasswords :: [Text] -> Int
countValidPasswords = length . filter (== True) . fmap (isValid . parsePassport)

data Passport = Passport
  { birthYear :: BirthYear,
    issueYear :: IssueYear,
    expirationYear :: ExpirationYear,
    height :: Height,
    hairColor :: HairColor,
    eyeColor :: EyeColor,
    passportId :: PassportId,
    countryId :: Maybe CountryId
  }
  deriving (Eq, Show)

instance Validation (Maybe Passport) where
  isValid (Just passport) = isValid passport
  isValid Nothing = False

instance Validation Passport where
  isValid Passport {..} =
    isValid birthYear
      && isValid issueYear
      && isValid expirationYear
      && isValid height
      && isValid hairColor
      && isValid eyeColor
      && isValid passportId

newtype BirthYear = BirthYear Integer
  deriving (Eq, Show)

instance Validation BirthYear where
  isValid (BirthYear birthYear) = birthYear >= 1000 && birthYear <= 2002

instance Parse BirthYear where
  parse values = BirthYear <$> parseInteger key values
    where
      key = pack "byr"

newtype IssueYear = IssueYear Integer
  deriving (Eq, Show)

instance Validation IssueYear where
  isValid (IssueYear issueYear) = issueYear <= 2020 && issueYear >= 2010

instance Parse IssueYear where
  parse values = IssueYear <$> parseInteger key values
    where
      key = pack "iyr"

newtype ExpirationYear = ExpirationYear Integer
  deriving (Eq, Show)

instance Validation ExpirationYear where
  isValid (ExpirationYear expirationYear) = expirationYear <= 2030 && expirationYear >= 2020

instance Parse ExpirationYear where
  parse values = ExpirationYear <$> parseInteger key values
    where
      key = pack "eyr"

data HeighUnit = Cm | In
  deriving (Eq, Show)

data Height = Height
  { unit :: HeighUnit,
    value :: Integer
  }
  deriving (Eq, Show)

instance Validation Height where
  isValid Height {..} = case unit of
    Cm -> value >= 150 && value <= 193
    In -> value >= 59 && value <= 76

instance Parse Height where
  parse values = case lookup key values of
    Nothing -> Left "Height not found"
    Just height -> case parseUnit height of
      Right unitValue ->
        Right $
          Height
            { unit = unitValue,
              value = read . unpack $ dropEnd 2 height
            }
      Left error -> Left error
    where
      key = pack "hgt"

parseUnit :: Text -> Either String HeighUnit
parseUnit height =
  case unit of
    "cm" -> Right Cm
    "in" -> Right In
    _ -> Left "Invalid height unit"
  where
    unit = unpack $ takeEnd 2 height

newtype HairColor = HairColor String
  deriving (Eq, Show)

instance Validation HairColor where
  isValid (HairColor ('#' : chars)) = length chars == 6 && all (`elem` validChars) chars
    where
      validChars = ['0' .. '9'] <> ['a' .. 'f']
  isValid _ = False

instance Parse HairColor where
  parse values = HairColor <$> parseString key values
    where
      key = pack "hcl"

newtype EyeColor = EyeColor String
  deriving (Eq, Show)

instance Validation EyeColor where
  isValid (EyeColor eyeColor) = eyeColor `elem` validEyeColors
    where
      validEyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

instance Parse EyeColor where
  parse values = EyeColor <$> parseString key values
    where
      key = pack "ecl"

newtype PassportId = PassportId String
  deriving (Eq, Show)

instance Validation PassportId where
  isValid (PassportId passportId) = length passportId == 9 && all (`elem` validChars) passportId
    where
      validChars = ['0' .. '9']

instance Parse PassportId where
  parse values = PassportId <$> parseString key values
    where
      key = pack "pid"

newtype CountryId = CountryId String
  deriving (Eq, Show)

instance Parse CountryId where
  parse values = CountryId <$> parseString key values
    where
      key = pack "cid"

parsePassport :: Text -> Maybe Passport
parsePassport value = case (eitherBirthYear, eitherIssueYear, eitherExpirationYear, eitherHeight, eitherHairColor, eitherEyeColor, eitherPassportId) of
  (Right birthYear, Right issueYear, Right expirationYear, Right height, Right hairColor, Right eyeColor, Right passportId) -> Just $ Passport {countryId = maybeCountryId, ..}
  _ -> Nothing
  where
    attributes :: [(Text, Text)] = map (Data.Bifunctor.second (replace (pack ":") empty)) $ breakOn (pack ":") <$> Data.Text.words value
    eitherBirthYear :: Either String BirthYear = parse attributes
    eitherIssueYear :: Either String IssueYear = parse attributes
    eitherExpirationYear :: Either String ExpirationYear = parse attributes
    eitherHeight :: Either String Height = parse attributes
    eitherHairColor :: Either String HairColor = parse attributes
    eitherEyeColor :: Either String EyeColor = parse attributes
    eitherPassportId :: Either String PassportId = parse attributes
    maybeCountryId :: Maybe CountryId = rightToMaybe $ parse attributes

rightToMaybe :: Either String CountryId -> Maybe CountryId
rightToMaybe (Right countryId) = Just countryId
rightToMaybe _ = Nothing

parseInteger :: Text -> [(Text, Text)] -> Either String Integer
parseInteger _ [] = Left "No value"
parseInteger key values = case lookup key values of
  Nothing -> Left $ unpack key <> " not found"
  Just value -> Right . read $ unpack value

parseString :: Text -> [(Text, Text)] -> Either String String
parseString _ [] = Left "No value"
parseString key values = case lookup key values of
  Nothing -> Left $ unpack key <> " not found"
  Just value -> Right $ unpack value
