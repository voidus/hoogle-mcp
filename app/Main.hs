module Main where

import MCP.Server
import MCP.Server.Derive
import MyLib (HoogleTool(..), handleHoogleTool)

main :: IO ()
main = runMcpServerStdio serverInfo handlers
  where
    serverInfo = McpServerInfo
      { serverName = "hoogle-mcp"
      , serverVersion = "0.1.0"
      , serverInstructions = "Hoogle search server for Haskell documentation"
      }
    handlers = McpServerHandlers
      { tools = Just $(deriveToolHandler ''HoogleTool 'handleHoogleTool)
      , prompts = Nothing
      , resources = Nothing
      }
