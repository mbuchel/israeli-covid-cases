{-# LANGUAGE OverloadedStrings #-}
{- |
Module          : $Header$
Description     : Website curl functions.
Copyright       : (c) Michael Buchel.
License         : BSD3

Maintainer      : michael@themimgroup.com
Stability       : experimental
Portability     : portable

Functions to communicate with the website.
| -}
module Website where

import           Schemas

import           Data.Aeson
import           Data.Text            ()
import           Network.HTTP.Client

import qualified Data.ByteString.Lazy as LBS

-- | The configured search request.
configuredSearch :: Int -- ^ Limits the size of the data returned
  -> DataRequest
configuredSearch size =
  DataRequest "8a21d39d-91e3-40db-aca1-f73f7ab1df69"
              ""
              Null
              True
              size
              0

-- | Performs the search to get all covid data from the website.
performSearch :: Manager -- ^ Manager for handling HTTPS requests.
  -> DataRequest -- ^ Configured search parameters
  -> IO [CovidRecord] -- ^ Returns a list of all possible covid cases
performSearch manager requestData = do
  request <- parseRequest "https://data.gov.il/api/3/action/datastore_search"
  let realRequest = request {
        method = "POST", requestBody = RequestBodyLBS $ encode requestData,
        requestHeaders = [
            ("Content-Type", "application/json")
        ]
  }
  response <- httpLbs realRequest manager
  case decode $ responseBody response :: Maybe WebResponse of
        Nothing -> return []
        Just x  -> return $ (records . result) x
