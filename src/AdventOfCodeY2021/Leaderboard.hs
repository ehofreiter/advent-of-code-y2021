{-# LANGUAGE OverloadedStrings #-}
module AdventOfCodeY2021.Leaderboard where

import Control.Applicative
import Data.Aeson
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

leaderboardFile = "data/leaderboard/dmjio-2021-12-22-02-15.json"

load :: IO Leaderboard
load = do
  leaderboardJson <- BSLC.pack <$> readFile leaderboardFile
  case eitherDecode leaderboardJson of
    Left err -> fail err
    Right lb -> pure lb

data Leaderboard = Leaderboard
  { owner_id :: MemberID
  , event :: String
  , members :: Map.Map MemberID Member
  }
  deriving (Eq, Ord, Show)

instance FromJSON Leaderboard where
  parseJSON = withObject "Leaderboard" $ \o -> Leaderboard
    <$> o .: "owner_id"
    <*> o .: "event"
    <*> o .: "members"

data Member = Member
  { id :: MemberID
  , local_score :: Int
  , last_star_ts :: Maybe Int
  , global_score :: Int
  , completion_day_level :: Map.Map AocDayID AocDay
  }
  deriving (Eq, Ord, Show)

type MemberID = String

instance FromJSON Member where
  parseJSON = withObject "Member" $ \o -> Member
    <$> o .: "id"
    <*> o .: "local_score"
    <*> (Just <$> o .: "last_star_ts" <|> pure Nothing)
    <*> o .: "global_score"
    <*> o .: "completion_day_level"

type AocDayID = String

type AocDay = Map.Map PartID Part

type PartID = String

newtype Part = Part
  { get_star_ts :: Int
  }
  deriving (Eq, Ord, Show)

instance FromJSON Part where
  parseJSON = withObject "Part" $ \o -> Part
    <$> o .: "get_star_ts"
