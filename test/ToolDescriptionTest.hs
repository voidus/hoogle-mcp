module ToolDescriptionTest where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Exception (bracket, catch)
import Data.Aeson (Value(..), decode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import ServerConfig (runServer)
import System.IO (Handle, IOMode(..), hClose, hFlush, hPutStrLn, readFile, stdin, stdout, withFile)
import System.IO.Temp (withSystemTempFile)
import Test.Tasty
import Test.Tasty.HUnit

-- | Run the MCP server main function with mocked stdin/stdout
runServerWithInput :: String -> IO String
runServerWithInput input = do
  -- Create temp files for stdin/stdout
  withSystemTempFile "stdin.txt" $ \stdinPath stdinHandle -> do
    withSystemTempFile "stdout.txt" $ \stdoutPath stdoutHandle -> do
      -- Write input to temp stdin and close it
      hPutStrLn stdinHandle input
      hFlush stdinHandle
      hClose stdinHandle

      -- Close the stdout handle so we can open it later
      hClose stdoutHandle

      -- Open the temp file for reading as stdin
      withFile stdinPath ReadMode $ \newStdin -> do
        withFile stdoutPath WriteMode $ \newStdout -> do
          -- Save original stdin/stdout
          bracket
            ( do
                oldStdin <- hDuplicate stdin
                oldStdout <- hDuplicate stdout
                return (oldStdin, oldStdout)
            )
            ( \(oldStdin, oldStdout) -> do
                -- Restore original stdin/stdout
                hDuplicateTo oldStdin stdin
                hDuplicateTo oldStdout stdout
                hClose oldStdin
                hClose oldStdout
            )
            ( \_ -> do
                -- Redirect stdin/stdout
                hDuplicateTo newStdin stdin
                hDuplicateTo newStdout stdout

                -- Run server in async, with timeout
                _ <- race_
                  (runServer `catch` (\(_ :: IOError) -> return ()))
                  (threadDelay 2000000) -- 2 second timeout

                return ()
            )

      -- Read output from temp file
      readFile stdoutPath

-- | Test that the MCP tools/list response contains proper descriptions
test_toolDescriptionsIncludeUsageGuidance :: TestTree
test_toolDescriptionsIncludeUsageGuidance = testCase "tools/list includes comprehensive usage guidance" $ do
  -- Send a tools/list request to the MCP server
  output <- runServerWithInput "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\",\"params\":{}}"

  -- Extract the JSON response line (filter out debug/error lines)
  let jsonLines = filter ("{\"id\":" `T.isPrefixOf`) $ T.lines $ T.pack output
  case jsonLines of
    [] -> assertFailure $ "No JSON response found in output: " ++ output
    (jsonLine:_) -> do
      let maybeResponse = decode @Value (BL8.pack $ T.unpack jsonLine)
      case maybeResponse of
        Nothing -> assertFailure "Failed to parse JSON response"
        Just response -> do
          -- Navigate to tools array
          let tools = extractTools response

          -- Find hoogle_search tool
          case findTool "hoogle_search" tools of
            Nothing -> assertFailure "hoogle_search tool not found"
            Just hoogleSearch -> do
              let desc = getDescription hoogleSearch

              -- Verify the description contains key usage guidance
              assertBool "Description should mention 'NAME SEARCH'"
                ("NAME SEARCH" `T.isInfixOf` desc)

              assertBool "Description should mention 'TYPE SEARCH'"
                ("TYPE SEARCH" `T.isInfixOf` desc)

              assertBool "Description should mention 'PACKAGE-QUALIFIED'"
                ("PACKAGE-QUALIFIED" `T.isInfixOf` desc)

              assertBool "Description should warn about combining multiple terms"
                ("Do NOT combine multiple" `T.isInfixOf` desc ||
                 "Do NOT combine" `T.isInfixOf` desc ||
                 "do not combine multiple" `T.isInfixOf` desc)

              assertBool "Description should provide BAD example"
                ("BAD:" `T.isInfixOf` desc)

              assertBool "Description should provide GOOD example"
                ("GOOD:" `T.isInfixOf` desc)

              assertBool "Description should mention 'ONE focused query'"
                ("ONE focused query" `T.isInfixOf` desc)

              -- Verify the specific bad example is mentioned
              assertBool "Description should mention the specific bad pattern"
                ("optparse-applicative many some argument" `T.isInfixOf` desc)

-- | Test that hoogle_docs also has appropriate guidance
test_hoogleDocsHasDescription :: TestTree
test_hoogleDocsHasDescription = testCase "hoogle_docs tool has description" $ do
  output <- runServerWithInput "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\",\"params\":{}}"

  let jsonLines = filter ("{\"id\":" `T.isPrefixOf`) $ T.lines $ T.pack output
  case jsonLines of
    [] -> assertFailure $ "No JSON response found in output: " ++ output
    (jsonLine:_) -> do
      let maybeResponse = decode @Value (BL8.pack $ T.unpack jsonLine)
      case maybeResponse of
        Nothing -> assertFailure "Failed to parse JSON response"
        Just response -> do
          let tools = extractTools response
          case findTool "hoogle_docs" tools of
            Nothing -> assertFailure "hoogle_docs tool not found"
            Just hoogleDocs -> do
              let desc = getDescription hoogleDocs
              assertBool "Description should not be empty" (not $ T.null desc)
              assertBool "Description should mention JSON" ("JSON" `T.isInfixOf` desc)
              assertBool "Description should warn about focused queries"
                ("ONE focused query" `T.isInfixOf` desc ||
                 "one focused query" `T.isInfixOf` desc)

-- Helper functions to navigate the JSON structure

extractTools :: Value -> V.Vector Value
extractTools (Object obj) =
  case KM.lookup "result" obj of
    Just (Object result) ->
      case KM.lookup "tools" result of
        Just (Array tools) -> tools
        _ -> V.empty
    _ -> V.empty
extractTools _ = V.empty

findTool :: Text -> V.Vector Value -> Maybe Value
findTool name tools = V.find matchesName tools
  where
    matchesName (Object obj) =
      case KM.lookup "name" obj of
        Just (String n) -> n == name
        _ -> False
    matchesName _ = False

getDescription :: Value -> Text
getDescription (Object obj) =
  case KM.lookup "description" obj of
    Just (String desc) -> desc
    _ -> ""
getDescription _ = ""
