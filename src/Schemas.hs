{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module          : $Header$
Description     : Schema definitions for Israeli Covid Cases.
Copyright       : (c) Michael Buchel.
License         : BSD3

Maintainer      : michael@themimgroup.com
Stability       : experimental
Portability     : portable

Below are the datastructures for JSON and CSV files.
| -}
module Schemas where

import           Data.Aeson
import           Data.Csv       (DefaultOrdered, FromNamedRecord, ToNamedRecord,
                                 header, headerOrder)
import           Data.Text      (Text)
import           Data.Text.Read
import           GHC.Generics

-- | This structure is what we will eventually send to the Israeli website
data DataRequest = DataRequest {
                        resourceId   :: Text, -- ^ Resource for the website.
                        query        :: Text, -- ^ Unnecessary.
                        drFilters    :: Value, -- ^ Filters on the data.
                        includeTotal :: Bool, -- ^ Set to true always.
                        dlimit       :: Int, -- ^ Maximum limit on response.
                        off          :: Int -- ^ Offset for how far in dataset.
                   }
        deriving (Show, Eq)

-- | This datastructure holds information regarding what fields are sent back.
-- This particular one is not useful, however is added to match the website.
data Field = Field {
                fieldType :: Text, -- ^ Type to cast into.
                fieldId   :: Text -- ^ Name of field.
             }
        deriving (Show, Eq)

-- | Datastructure that is holding the records, this one is both JSON and
-- CSV formats.
data CovidRecord = CovidRecord {
                        uid                   :: !Int,
                        city                  :: !Text,
                        cityCode              :: !Text,
                        date                  :: !Text,
                        cumulativeVerified    :: !Int,
                        cumulativeRecovered   :: !Int,
                        cumulativeDeaths      :: !Int,
                        cumulativeTests       :: !Int,
                        cumulativeDiagnostics :: !Int
                   }
        deriving (Show, Generic, Eq)

-- | Links datastructure saying where to go next.
data WebLinks = WebLinks {
                        start :: Text,
                        next  :: Text
                }
        deriving (Show, Generic, Eq)

-- | Result substructure in WebResponse.
data WebResult = WebResult {
                        include_total  :: Bool,
                        resource_id    :: Text,
                        fields         :: [Field],
                        records_format :: Text,
                        q              :: Text,
                        records        :: [CovidRecord],
                        limit          :: Int,
                        offset         :: Int,
                        _links         :: WebLinks,
                        total          :: Int
                 }
        deriving (Show, Generic, Eq)

-- | Datastructure that is holding the response.
data WebResponse = WebResponse {
                        help    :: Text,
                        success :: Bool,
                        result  :: WebResult
                   }
        deriving (Show, Generic, Eq)

-- | In case we need to do processing on the records after we get them.
instance FromNamedRecord CovidRecord

-- | Writes to CSV File.
instance ToNamedRecord CovidRecord

-- | Header for the CovidRecord.
instance DefaultOrdered CovidRecord where
  headerOrder _ = header [
                        "uid", "city", "cityCode", "date",
                        "cumulativeVerified", "cumulativeRecovered",
                        "cumulativeDeaths", "cumulativeTests",
                        "cumulativeDiagnostics"
                  ]

-- | Convert from JSON into DataRequest structure.
instance FromJSON DataRequest where
  parseJSON = withObject "DataRequest" $ \v -> DataRequest
    <$> v .: "resource_id"
    <*> v .: "q"
    <*> v .: "filters"
    <*> v .: "include_total"
    <*> v .: "limit"
    <*> v .: "offset"

-- | Converts from JSON into field substructure.
instance FromJSON Field where
  parseJSON = withObject "Field" $ \v -> Field
    <$> v .: "type"
    <*> v .: "id"

-- | Converts the text to an integer, or -1 if it is a failure.
convertToInt :: Text -> Int
convertToInt t = case decimal t of
  Left _         -> -1
  Right (num, _) -> num

-- | Covid Record being transformed from the JSON passed by the website.
instance FromJSON CovidRecord where
  parseJSON = withObject "CovidRecord" $ \v -> do
    u <- v .: "_id"
    cty <- v .: "City_Name"
    code <- v .: "City_Code"
    d <- v .: "Date"
    verified <- v .: "Cumulative_verified_cases"
    recovered <- v .: "Cumulated_recovered"
    deaths <- v .: "Cumulated_deaths"
    tests <- v .: "Cumulated_number_of_tests"
    diagnostics <- v .: "Cumulated_number_of_diagnostic_tests"
    return $ CovidRecord u
                         cty
                         code
                         d
                         (convertToInt verified)
                         (convertToInt recovered)
                         (convertToInt deaths)
                         (convertToInt tests)
                         (convertToInt diagnostics)

instance FromJSON WebLinks
instance FromJSON WebResult
instance FromJSON WebResponse

-- | Convert from DataRequest structure to JSON.
instance ToJSON DataRequest where
  toJSON (DataRequest resource sq sfilters t slimit soffset) =
    object [
      "resource_id" .= resource,
      "q" .= sq,
      "filters" .= sfilters,
      "include_total" .= t,
      "limit" .= slimit,
      "offset" .= soffset
    ]
