{- |
Module          : $Header$
Description     : Application for the Israeli Covid Cases.
Copyright       : (c) Michael Buchel.
License         : BSD3

Maintainer      : michael@themimgroup.com
Stability       : experimental
Portability     : portable

Main program which will provide us with the code to get the records from the website.
| -}
module Main where

import           Schemas
import           Website

import           Data.Csv
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Environment

import qualified Data.ByteString.Lazy    as LBS

main :: IO ()
main = do
  records <-
    newManager tlsManagerSettings >>=
        \x -> performSearch x $ configuredSearch 100000000000
  let csv = encodeDefaultOrderedByName records
  LBS.writeFile "israeli-covid-cases.csv" csv
