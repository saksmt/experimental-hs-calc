module Term.Parser
    ( termsP
    ) where

import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number(floating2)

import Control.Monad
import Data.Maybe(isJust)

import Term.Def(Term(..))

eP = spaces >> oneOf "eE" >> spaces >> return ET
piP = spaces >> (string "PI" <|> string "pi" <|> string "Pi" <|> string "π") >> spaces >> return PiT
valueP = do
    spaces
    v <- floating2 True
    spaces
    return $ ValueT v

funP :: String -> ([Term] -> Term) -> Parser Term
funP functionName constructor = do
    spaces
    string functionName
    spaces
    char '('
    spaces
    terms <- termsP <?> "expression or value"
    spaces
    char ')'
    spaces
    return $ constructor terms

absMP = do
    spaces
    char '|'
    spaces
    terms <- termsP <?> "expression or value"
    spaces
    char '|'
    spaces
    return $ AbsT terms

logP = do
    spaces
    string "log"
    spaces
    char '('
    base <- termsP
    spaces
    char ','
    spaces
    exponent <- termsP
    spaces
    char ')'
    spaces
    return $ LogT base exponent

binaryP :: Char -> Term -> Parser Term
binaryP sign v = do
    spaces
    char sign
    spaces
    return v

sumP = binaryP '+' SumT
difP = binaryP '-' DiffT
powP = binaryP '^' PowT
mulP = (binaryP '*' MulT) <|> (binaryP '×' MulT) <|> try (binaryP 'x' MulT)
divP = (binaryP '/' DivT) <|> (binaryP '÷' DivT)
modP = binaryP '%' ModT

floorP = funP "floor" FloorT
ceilP = funP "ceil" CeilT
prioP = funP "" PrioT
roundP = funP "round" RoundT
absP = funP "abs" AbsT
sinP = funP "sin" SinT
cosP = funP "cos" CosT
tanP = funP "tan" TanT
asinP = funP "asin" ASinT
acosP = funP "acos" ACosT
atanP = funP "atan" ATanT
sqrtP = funP "sqrt" SqrtT
expP = funP "exp" ExpT

opP =  sumP
   <|> difP
   <|> powP
   <|> mulP
   <|> divP
   <|> modP

_unaryP = valueP
       <|> try piP
       <|> try expP
       <|> try eP
       <|> try prioP
       <|> try ceilP
       <|> floorP
       <|> roundP
       <|> absMP
       <|> try absP
       <|> try sinP
       <|> try cosP
       <|> tanP
       <|> try asinP
       <|> try acosP
       <|> try atanP
       <|> try sqrtP
       <|> logP

unaryP = do
    s <- optionMaybe $ char '-'
    v <- _unaryP
    if isJust s then
        return $ NegT v
    else
        return v

afterUnaryP = do
    o <- opP <?> "operator"
    a <- unaryP <?> "expression or value"
    return [o, a]

termsP = do
    u <- unaryP <?> "expression or value"
    a <- many afterUnaryP
    return $ u:(join a)

