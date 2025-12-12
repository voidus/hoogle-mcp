module Hoogle (HoogleTool(..), handleHoogleTool) where

import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server (Content(..))
import System.Process (readProcess)

-- | Hoogle search tools for querying Haskell documentation.
--
-- Hoogle supports several query types:
--
-- 1. NAME SEARCH: Search for a function, type, or module by name
--    Examples: \"map\", \"foldr\", \"Maybe\", \"Data.List\"
--
-- 2. TYPE SEARCH: Search by type signature using Haskell syntax
--    Examples: \"(a -> b) -> [a] -> [b]\", \"a -> Maybe a\", \"Int -> String\"
--
-- 3. PACKAGE-QUALIFIED SEARCH: Limit search to a specific package using \"+package\"
--    Examples: \"+base map\", \"+text concat\", \"+optparse-applicative many\"
--
-- IMPORTANT BEST PRACTICES:
-- - Make ONE focused query at a time
-- - Do NOT combine multiple unrelated terms in a single query
-- - BAD: \"optparse-applicative many some argument\" (too many terms)
-- - GOOD: \"+optparse-applicative many\" then \"+optparse-applicative some\" (separate queries)
-- - For type searches, use proper Haskell type syntax with arrows and parentheses
-- - Use package qualification when you know which package you're looking for
data HoogleTool
  = HoogleSearch
      { query :: Text  -- ^ A single focused Hoogle query (name, type signature, or package-qualified search). Keep it simple and focused.
      }
  | HoogleDocs
      { query :: Text  -- ^ A single focused Hoogle query (name, type signature, or package-qualified search). Keep it simple and focused.
      , count :: Maybe Int  -- ^ Maximum number of results to return (default: 10)
      }
  deriving (Show)

-- Handler for the Hoogle search tool
handleHoogleTool :: HoogleTool -> IO Content
handleHoogleTool (HoogleSearch q) = do
  -- Call 'hoogle search -- <query>' and capture output
  result <- readProcess "hoogle" ["search", "--", T.unpack q] ""
  pure $ ContentText $ T.pack result

handleHoogleTool (HoogleDocs q maybeCount) = do
  -- Call 'hoogle search --json -- <query>' to get structured documentation
  let countArg = case maybeCount of
                   Just n -> ["-n", show n]
                   Nothing -> []
      args = ["search", "--json"] ++ countArg ++ ["--", T.unpack q]
  result <- readProcess "hoogle" args ""
  pure $ ContentText $ T.pack result
