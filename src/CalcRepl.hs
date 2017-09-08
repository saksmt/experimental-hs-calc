module CalcRepl where

import Term
import Expr
import Text.Parsec

import Control.Monad
import System.IO

replBody raw = either ((hPutStrLn stderr) . show) print $ fmap (eval . toExpr) $ parse termsP "(interactive)" raw

