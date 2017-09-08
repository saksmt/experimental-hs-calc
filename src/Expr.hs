{-# LANGUAGE TemplateHaskell #-}

module Expr(Expr(..), eval) where

import Data.List(findIndex)
import Data.Fixed(mod')

data Expr = ValueF Double
          | Sum Expr Expr
          | Diff Expr Expr
          | Pow Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Neg Expr
          | Mod Expr Expr
          | Round Expr
          | Floor Expr
          | Abs Expr
          | Sin Expr
          | Cos Expr
          | Tan Expr
          | ASin Expr
          | ACos Expr
          | ATan Expr
          | Pi
          | Sqrt Expr
          | Log Expr Expr
          | Exp Expr
          | E
          | Ceil Expr deriving (Show)

eval :: Expr -> Double
eval (ValueF v) = v
eval (Sum a b) = eval a + eval b
eval (Diff a b) = eval a - eval b
eval (Pow a b) = eval a ** eval b
eval (Mul a b) = eval a * eval b
eval (Neg v) = - eval v
eval (Mod a b) = eval a `mod'` eval b
eval (Round v) = fromIntegral $ round $ eval v
eval (Floor v) = fromIntegral $ floor $ eval v
eval (Ceil v) = fromIntegral $ ceiling $ eval v
eval (Div a b) = eval a / eval b
eval (Abs v) = abs $ eval v
eval (Sin v) = sin $ eval v
eval (Cos v) = cos $ eval v
eval (Tan v) = tan $ eval v
eval (ASin v) = asin $ eval v
eval (ACos v) = acos $ eval v
eval (ATan v) = atan $ eval v
eval (Pi) = pi
eval (Sqrt v) = sqrt $ eval v
eval (Log b e) = eval b `logBase` eval e
eval (Exp v) = exp $ eval v
eval (E) = exp 1

