module ServerConfig (serverInfo, handlers, runServer) where

import MCP.Server
import MCP.Server.Derive
import Hoogle (HoogleTool(..), handleHoogleTool)

serverInfo :: McpServerInfo
serverInfo = McpServerInfo
  { serverName = "hoogle-mcp"
  , serverVersion = "0.1.0"
  , serverInstructions = "Hoogle search server for Haskell documentation"
  }

handlers :: McpServerHandlers IO
handlers = McpServerHandlers
  { tools = Just $(deriveToolHandlerWithDescription ''HoogleTool 'handleHoogleTool
      [ ("HoogleSearch", unlines
          [ "Search Haskell documentation using Hoogle."
          , ""
          , "CRITICAL: Hoogle is NOT like Google. Do NOT use natural language queries!"
          , "BAD: \"how to parse JSON\", \"convert string to int\", \"read a file\""
          , "GOOD: \"parseJSON\", \"Text -> Int\", \"FilePath -> IO ByteString\""
          , ""
          , "Common use cases:"
          , "- Look up a function's type: just use the function name (\"foldl\", \"traverse\")"
          , "- Find functions by type: use type signatures (\"[a] -> Int\", \"Maybe a -> a\")"
          , "- Find similar functions: search by partial type (\"a -> Maybe b\")"
          , ""
          , "Hoogle supports three query types:"
          , ""
          , "1. NAME SEARCH: Search for a function, type, or module by name"
          , "   Examples: \"map\", \"foldr\", \"Maybe\", \"Data.List\", \"parseJSON\""
          , "   Use this to look up a specific function's type signature and documentation"
          , ""
          , "2. TYPE SEARCH: Search by type signature using Haskell syntax"
          , "   Examples: \"(a -> b) -> [a] -> [b]\", \"Text -> ByteString\", \"FilePath -> IO String\""
          , ""
          , "3. PACKAGE-QUALIFIED: Limit search to a specific package using \"+package\""
          , "   Examples: \"+base map\", \"+text concat\", \"+aeson parseJSON\""
          , ""
          , "IMPORTANT: Make ONE focused query at a time. Do NOT combine multiple"
          , "unrelated terms. BAD: \"optparse-applicative many some argument\""
          , "GOOD: \"+optparse-applicative many\" (then make separate queries for other terms)"
          ])
      , ("HoogleDocs", unlines
          [ "Search Haskell documentation with JSON results for structured output."
          , ""
          , "CRITICAL: Use Haskell syntax, NOT natural language!"
          , "BAD: \"how to parse JSON\" - GOOD: \"parseJSON\" or \"ByteString -> Value\""
          , ""
          , "Common use cases:"
          , "- Look up a function's type and docs: use the function name (\"traverse\", \"foldMap\")"
          , "- Find functions by type signature: \"[a] -> Int\", \"Text -> ByteString\""
          , ""
          , "Same query syntax as HoogleSearch: name search, type signature search,"
          , "or +package qualified. Returns detailed documentation in JSON format."
          , ""
          , "Make ONE focused query at a time - do not combine multiple unrelated terms."
          ])
      ])
  , prompts = Nothing
  , resources = Nothing
  }

-- | Run the MCP server with stdio transport
runServer :: IO ()
runServer = runMcpServerStdio serverInfo handlers
