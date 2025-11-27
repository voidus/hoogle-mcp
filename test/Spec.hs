module Spec where

import Control.Exception
import Data.Function
import Discover qualified
import System.Console.ANSI
import System.Environment (getArgs, withArgs)

main :: IO ()
main = do
  Discover.main

cliMain :: IO ()
cliMain =
  let
    selectTest =
      Nothing
        -- Just "/expires sessions after 90 days/"
        & maybe id (\t rest -> "-p" : t : rest)
   in
    do
      args <- getArgs
      withArgs
        (selectTest ("--hide-successes" : "--color=always" : args))
        main
      `finally` do
        setSGR [Reset]
        -- For some messed up reason this doesn't work, so we just go up and down
        -- a line instead
        -- hFlush stdout
        putStrLn "\x1b[A"
