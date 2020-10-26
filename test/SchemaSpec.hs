{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{- |
Module          : $Header$
Description     : Test Specs for Schemas.
Copyright       : (c) Michael Buchel.
License         : BSD3

Maintainer      : michael@themimgroup.com
Stability       : experimental
Portability     : portable

Spec to test out that schemas work as they should
| -}
module SchemaSpec (spec) where

import           Schemas

import           Data.Aeson
import           Test.Hspec


spec :: Spec
spec = parallel $ do
  describe "DataRequest" $ do
    it "Proper filter is null JSON" $ do
      let request = "{\"resource_id\": \"8a21d39d-91e3-40db-aca1-f73f7ab1df69\",\"q\": \"\",\"filters\": {},\"include_total\": true,\"limit\": 100,\"offset\": 0}"
      (decode request :: Maybe DataRequest) `shouldNotBe` Nothing

    it "Proper filter is not null JSON" $ do
      let request = "{\"resource_id\": \"8a21d39d-91e3-40db-aca1-f73f7ab1df69\",\"q\": \"\",\"filters\": {\"hello\": \"goodbye\"},\"include_total\": true,\"limit\": 100,\"offset\": 0}"
      (decode request :: Maybe DataRequest) `shouldNotBe` Nothing

    it "Resource id is number instead" $ do
      let request = "{\"resource_id\": 0, \"q\": \"\",\"filters\": {},\"include_total\": true,\"limit\": 100,\"offset\": 0}"
      (decode request :: Maybe DataRequest) `shouldBe` Nothing

  describe "Field" $ do
    it "Proper Field JSON" $ do
      let request = "{\"type\": \"hi hi hi\", \"id\": \"hi\"}"
      (decode request :: Maybe Field) `shouldNotBe` Nothing

  describe "CovidRecord" $ do
    it "Proper CovidRecord JSON" $ do
      let request = "{\"_id\":1,\"City_Name\":\"\215\144\215\145\215\149 \215\146'\215\149\215\149\215\153\215\153\215\162\215\147 (\215\169\215\145\215\152)\",\"City_Code\":\"967\",\"Date\":\"2020-03-11\",\"Cumulative_verified_cases\":\"0\",\"Cumulated_recovered\":\"0\",\"Cumulated_deaths\":\"0\",\"Cumulated_number_of_tests\":\"0\",\"Cumulated_number_of_diagnostic_tests\":\"0\"}"
      (decode request :: Maybe CovidRecord) `shouldNotBe` Nothing

    it "Improper id CovidRecord JSON" $ do
      let request = "{\"_id\":\"1\",\"City_Name\":\"\215\144\215\145\215\149 \215\146'\215\149\215\149\215\153\215\153\215\162\215\147 (\215\169\215\145\215\152)\",\"City_Code\":\"967\",\"Date\":\"2020-03-11\",\"Cumulative_verified_cases\":\"0\",\"Cumulated_recovered\":\"0\",\"Cumulated_deaths\":\"0\",\"Cumulated_number_of_tests\":\"0\",\"Cumulated_number_of_diagnostic_tests\":\"0\"}"
      (decode request :: Maybe CovidRecord) `shouldBe` Nothing

  describe "WebLinks" $ do
    it "Proper WebLinks JSON" $ do
      let request = "{\"start\": \"/api/3/action/datastore_search\", \"next\": \"/api/3/action/datastore_search?offset=1\"}"
      (decode request :: Maybe WebLinks) `shouldNotBe` Nothing

    it "Improper Weblinks JSON" $ do
      let request = "{\"start\": 0, \"next\": \"/api/3/action/datastore_search?offset=1\"}"
      (decode request :: Maybe WebLinks) `shouldBe` Nothing

  describe "WebResult" $ do
    it "Proper WebResult empty arrays JSON" $ do
      let request = "{\"include_total\": true, \"resource_id\": \"8a21d39d-91e3-40db-aca1-f73f7ab1df69\", \"fields\": [], \"records_format\": \"objects\", \"q\": \"\", \"records\": [], \"limit\": 1, \"offset\": 0, \"_links\": {\"start\": \"/api/3/action/datastore_search\", \"next\": \"/api/3/action/datastore_search?offset=1\"}, \"total\": 61788}"
      (decode request :: Maybe WebResult) `shouldNotBe` Nothing

    it "Proper WebResult correct arrays JSON" $ do
      let request = "{\"include_total\": true, \"resource_id\": \"8a21d39d-91e3-40db-aca1-f73f7ab1df69\", \"fields\": [{\"type\": \"int\", \"id\": \"_id\"}, {\"type\": \"text\", \"id\": \"City_Name\"}, {\"type\": \"text\", \"id\": \"City_Code\"}, {\"type\": \"text\", \"id\": \"Date\"}, {\"type\": \"text\", \"id\": \"Cumulative_verified_cases\"}, {\"type\": \"text\", \"id\": \"Cumulated_recovered\"}, {\"type\": \"text\", \"id\": \"Cumulated_deaths\"}, {\"type\": \"text\", \"id\": \"Cumulated_number_of_tests\"}, {\"type\": \"text\", \"id\": \"Cumulated_number_of_diagnostic_tests\"}], \"records_format\": \"objects\", \"q\": \"\", \"records\": [{\"_id\":1,\"City_Name\":\"\215\144\215\145\215\149 \215\146'\215\149\215\149\215\153\215\153\215\162\215\147 (\215\169\215\145\215\152)\",\"City_Code\":\"967\",\"Date\":\"2020-03-11\",\"Cumulative_verified_cases\":\"0\",\"Cumulated_recovered\":\"0\",\"Cumulated_deaths\":\"0\",\"Cumulated_number_of_tests\":\"0\",\"Cumulated_number_of_diagnostic_tests\":\"0\"}], \"limit\": 1, \"offset\": 0, \"_links\": {\"start\": \"/api/3/action/datastore_search\", \"next\": \"/api/3/action/datastore_search?offset=1\"}, \"total\": 61788}"
      (decode request :: Maybe WebResult) `shouldNotBe` Nothing

  describe "WebResponse" $ do
    it "Proper response JSON" $ do
      let request = "{\"help\": \"https://data.gov.il/api/3/action/help_show?name=datastore_search\", \"success\": true, \"result\": {\"include_total\": true, \"resource_id\": \"8a21d39d-91e3-40db-aca1-f73f7ab1df69\", \"fields\": [{\"type\": \"int\", \"id\": \"_id\"}, {\"type\": \"text\", \"id\": \"City_Name\"}, {\"type\": \"text\", \"id\": \"City_Code\"}, {\"type\": \"text\", \"id\": \"Date\"}, {\"type\": \"text\", \"id\": \"Cumulative_verified_cases\"}, {\"type\": \"text\", \"id\": \"Cumulated_recovered\"}, {\"type\": \"text\", \"id\": \"Cumulated_deaths\"}, {\"type\": \"text\", \"id\": \"Cumulated_number_of_tests\"}, {\"type\": \"text\", \"id\": \"Cumulated_number_of_diagnostic_tests\"}], \"records_format\": \"objects\", \"q\": \"\", \"records\": [{\"_id\":1,\"City_Name\":\"\215\144\215\145\215\149 \215\146'\215\149\215\149\215\153\215\153\215\162\215\147 (\215\169\215\145\215\152)\",\"City_Code\":\"967\",\"Date\":\"2020-03-11\",\"Cumulative_verified_cases\":\"0\",\"Cumulated_recovered\":\"0\",\"Cumulated_deaths\":\"0\",\"Cumulated_number_of_tests\":\"0\",\"Cumulated_number_of_diagnostic_tests\":\"0\"}], \"limit\": 1, \"offset\": 0, \"_links\": {\"start\": \"/api/3/action/datastore_search\", \"next\": \"/api/3/action/datastore_search?offset=1\"}, \"total\": 61788}}"
      (decode request :: Maybe WebResponse) `shouldNotBe` Nothing
