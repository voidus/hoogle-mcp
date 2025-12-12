module HoogleToolTest where

import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server (Content(..))
import Hoogle (HoogleTool(..), handleHoogleTool)
import Test.Tasty
import Test.Tasty.HUnit

test_hoogleSearchReturnsResults :: TestTree
test_hoogleSearchReturnsResults = testCase "hoogle search returns results for 'map'" $ do
  let tool = HoogleSearch { query = "map" }
  result <- handleHoogleTool tool
  case result of
    ContentText text -> do
      -- Verify that the result contains "map" (the search term)
      assertBool "Result should contain 'map'" (T.isInfixOf "map" (T.toLower text))
      -- Verify that the result is not empty
      assertBool "Result should not be empty" (not $ T.null text)
    _ -> assertFailure "Expected ContentText result"

test_hoogleSearchHandlesComplexQuery :: TestTree
test_hoogleSearchHandlesComplexQuery = testCase "hoogle search handles 'intercalate' query" $ do
  let tool = HoogleSearch { query = "intercalate" }
  result <- handleHoogleTool tool
  case result of
    ContentText text -> do
      -- Verify that the result contains expected modules/functions
      assertBool "Result should contain 'Data.List'" (T.isInfixOf "Data.List" text)
      assertBool "Result should contain 'intercalate'" (T.isInfixOf "intercalate" text)
      assertBool "Result should not be empty" (not $ T.null text)
    _ -> assertFailure "Expected ContentText result"

test_hoogleDocsReturnsJsonForFunction :: TestTree
test_hoogleDocsReturnsJsonForFunction = testCase "hoogle docs returns JSON documentation for 'map'" $ do
  let tool = HoogleDocs { query = "map", count = Just 2 }
  result <- handleHoogleTool tool
  case result of
    ContentText text -> do
      -- Verify that the result is JSON (contains expected JSON structure markers)
      assertBool "Result should contain JSON array start" (T.isPrefixOf "[" (T.strip text))
      assertBool "Result should contain 'docs' field" (T.isInfixOf "\"docs\"" text)
      assertBool "Result should contain 'item' field" (T.isInfixOf "\"item\"" text)
      assertBool "Result should contain 'module' field" (T.isInfixOf "\"module\"" text)
      assertBool "Result should contain 'url' field" (T.isInfixOf "\"url\"" text)
      assertBool "Result should not be empty" (not $ T.null text)
    _ -> assertFailure "Expected ContentText result"

test_hoogleDocsReturnsModuleDocumentation :: TestTree
test_hoogleDocsReturnsModuleDocumentation = testCase "hoogle docs returns documentation for 'Data.List' module" $ do
  let tool = HoogleDocs { query = "Data.List", count = Just 1 }
  result <- handleHoogleTool tool
  case result of
    ContentText text -> do
      -- Verify that the result contains module information
      assertBool "Result should contain 'module Data.List'" (T.isInfixOf "module Data.List" text)
      assertBool "Result should contain module type" (T.isInfixOf "\"type\":\"module\"" text || T.isInfixOf "\"type\": \"module\"" text)
      assertBool "Result should contain URL" (T.isInfixOf "\"url\"" text)
      assertBool "Result should not be empty" (not $ T.null text)
    _ -> assertFailure "Expected ContentText result"

test_hoogleDocsWithDefaultCount :: TestTree
test_hoogleDocsWithDefaultCount = testCase "hoogle docs works with default count (Nothing)" $ do
  let tool = HoogleDocs { query = "fmap", count = Nothing }
  result <- handleHoogleTool tool
  case result of
    ContentText text -> do
      -- Verify that the result is valid JSON and contains results
      assertBool "Result should contain JSON array start" (T.isPrefixOf "[" (T.strip text))
      assertBool "Result should contain 'fmap'" (T.isInfixOf "fmap" text)
      assertBool "Result should not be empty" (not $ T.null text)
    _ -> assertFailure "Expected ContentText result"
