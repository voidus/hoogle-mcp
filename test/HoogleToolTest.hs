module HoogleToolTest where

import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server (Content(..))
import MyLib (HoogleTool(..), handleHoogleTool)
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
