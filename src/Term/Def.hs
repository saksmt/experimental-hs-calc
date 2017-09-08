{-# LANGUAGE TemplateHaskell #-}

module Term.Def
    ( Term(..)
    , isSumT
    , isDiffT
    , isPowT
    , isMulT
    , isDivT
    , isModT
    , isNegT
    , isPrioT
    , isRoundT
    , isFloorT
    , isCeilT
    , isAbsT
    , isSinT
    , isCosT
    , isTanT
    , isASinT
    , isACosT
    , isATanT
    , isPiT
    , isSqrtT
    , isLogT
    , isExpT
    , isET
    ) where

import Data.DeriveTH
import Data.Derive.Is

data Term = ValueT Double
          | SumT
          | DiffT
          | PowT
          | MulT
          | DivT
          | ModT
          | NegT Term
          | PrioT [Term]
          | RoundT [Term]
          | FloorT [Term]
          | AbsT [Term]
          | SinT [Term]
          | CosT [Term]
          | TanT [Term]
          | ASinT [Term]
          | ACosT [Term]
          | ATanT [Term]
          | PiT
          | SqrtT [Term]
          | LogT [Term] [Term]
          | ExpT [Term]
          | ET
          | CeilT [Term] deriving (Show)

$( derive makeIs ''Term )

