module Main where

import MCP.Server
import MCP.Server.Derive
import MyLib (HoogleTool(..), handleHoogleTool)
import Options.Applicative
import Data.Version (showVersion)
import Paths_hoogle_mcp (version)

data Command
  = RunServer
  | ShowVersion

commandParser :: Parser Command
commandParser = versionFlag <|> pure RunServer
  where
    versionFlag = flag' ShowVersion
      ( long "version"
     <> short 'v'
     <> help "Show version information"
      )

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc
 <> progDesc "MCP server for Hoogle (Haskell documentation search)"
 <> header "hoogle-mcp - search Haskell documentation via MCP"
  )

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    ShowVersion -> putStrLn $ "hoogle-mcp version " ++ showVersion version
    RunServer -> runMcpServerStdio serverInfo handlers
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
