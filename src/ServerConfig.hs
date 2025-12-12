module ServerConfig (serverInfo, handlers, runServer) where

import MCP.Server
import MCP.Server.Derive
import MyLib (HoogleTool(..), handleHoogleTool)

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
          , "Hoogle supports several query types:"
          , "1. NAME SEARCH: Search for a function, type, or module by name"
          , "   Examples: \"map\", \"foldr\", \"Maybe\", \"Data.List\""
          , ""
          , "2. TYPE SEARCH: Search by type signature using Haskell syntax"
          , "   Examples: \"(a -> b) -> [a] -> [b]\", \"a -> Maybe a\", \"Int -> String\""
          , ""
          , "3. PACKAGE-QUALIFIED: Limit search to a specific package using \"+package\""
          , "   Examples: \"+base map\", \"+text concat\", \"+optparse-applicative many\""
          , ""
          , "IMPORTANT: Make ONE focused query at a time. Do NOT combine multiple"
          , "unrelated terms. BAD: \"optparse-applicative many some argument\""
          , "GOOD: \"+optparse-applicative many\" (then make separate queries for other terms)"
          ])
      , ("HoogleDocs", unlines
          [ "Search Haskell documentation with JSON results for structured output."
          , "Same query syntax as HoogleSearch. Returns detailed documentation."
          , ""
          , "Query types: name search, type signature search, or +package qualified."
          , "Make ONE focused query at a time - do not combine multiple unrelated terms."
          ])
      ])
  , prompts = Nothing
  , resources = Nothing
  }

-- | Run the MCP server with stdio transport
runServer :: IO ()
runServer = runMcpServerStdio serverInfo handlers
