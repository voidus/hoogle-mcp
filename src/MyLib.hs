module MyLib (HoogleTool(..), handleHoogleTool) where

import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server (Content(..))
import System.Process (readProcess)

-- Define the Hoogle search tool
data HoogleTool = HoogleSearch
  { query :: Text
  } deriving (Show)

-- Handler for the Hoogle search tool
handleHoogleTool :: HoogleTool -> IO Content
handleHoogleTool (HoogleSearch q) = do
  -- Call 'hoogle search -- <query>' and capture output
  result <- readProcess "hoogle" ["search", "--", T.unpack q] ""
  pure $ ContentText $ T.pack result
