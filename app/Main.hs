module Main where

import CalcRepl

import Control.Monad
import System.IO
import System.Console.Repline
import Control.Monad.Trans

main :: IO ()
main = evalRepl "> " (\x -> liftIO $ replBody x) [] (Word $ const $ return []) (return ())
