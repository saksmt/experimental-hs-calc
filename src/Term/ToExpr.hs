module Term.ToExpr
    ( toExpr
    , prioritize
    ) where

import Util.List(slice, replace)
import Util.Predicate(orP)

import Term.Def
import Expr

import Data.List(findIndex)

prioritizeF terms = maybe terms doPrioritize $ findIndex isPowT terms
    where doPrioritize idx = prioritizeF $ replace (idx - 1, idx + 1) (PrioT $ slice (idx - 1, 3) terms) terms

prioritizeS terms = maybe terms doPrioritize $ findIndex (isMulT `orP` isDivT `orP` isModT) terms
    where doPrioritize idx = prioritizeS $ replace (idx - 1, idx + 1) (PrioT $ slice (idx - 1, 3) terms) terms

prioritize terms = prioritizeF $ prioritizeS $ map prioritizeInner terms
    where prioritizeInner (CeilT ts) = CeilT $ prioritize ts 
          prioritizeInner (FloorT ts) = FloorT $ prioritize ts 
          prioritizeInner (RoundT ts) = RoundT $ prioritize ts 
          prioritizeInner (PrioT ts) = PrioT $ prioritize ts 
          prioritizeInner (AbsT ts) = AbsT $ prioritize ts
          prioritizeInner (SinT ts) = SinT $ prioritize ts
          prioritizeInner (CosT ts) = CosT $ prioritize ts
          prioritizeInner (TanT ts) = TanT $ prioritize ts
          prioritizeInner (ASinT ts) = ASinT $ prioritize ts
          prioritizeInner (ACosT ts) = ACosT $ prioritize ts
          prioritizeInner (ATanT ts) = ATanT $ prioritize ts
          prioritizeInner (SqrtT ts) = SqrtT $ prioritize ts
          prioritizeInner (ExpT ts) = ExpT $ prioritize ts
          prioritizeInner (LogT b e) = LogT (prioritize b) (prioritize e)
          prioritizeInner (NegT t) = NegT $ prioritizeInner t
          prioritizeInner x = x

prioritizedToExpr :: Expr -> [Term] -> Expr
prioritizedToExpr previous (o:b:xs) = let sa = termToExpr b in case o of 
    SumT -> prioritizedToExpr (Sum previous sa) xs 
    DiffT -> prioritizedToExpr (Diff previous sa) xs 
    MulT -> prioritizedToExpr (Mul previous sa) xs 
    DivT -> prioritizedToExpr (Div previous sa) xs 
    ModT -> prioritizedToExpr (Mod previous sa) xs 
    PowT -> prioritizedToExpr (Pow previous sa) xs 
    otherwise -> error "Invalid expression given"

prioritizedToExpr previous _ = previous

termToExpr (ValueT v) = ValueF v
termToExpr (PiT) = Pi
termToExpr (ET) = E
termToExpr (PrioT terms) = termsToExpr terms
termToExpr (CeilT terms) = Ceil $ termsToExpr terms
termToExpr (FloorT terms) = Floor $ termsToExpr terms
termToExpr (RoundT terms) = Round $ termsToExpr terms
termToExpr (AbsT terms) = Abs $ termsToExpr terms
termToExpr (ExpT terms) = Exp $ termsToExpr terms
termToExpr (SinT terms) = Sin $ termsToExpr terms
termToExpr (CosT terms) = Cos $ termsToExpr terms
termToExpr (TanT terms) = Tan $ termsToExpr terms
termToExpr (ASinT terms) = ASin $ termsToExpr terms
termToExpr (ACosT terms) = ACos $ termsToExpr terms
termToExpr (ATanT terms) = ATan $ termsToExpr terms
termToExpr (SqrtT terms) = Sqrt $ termsToExpr terms
termToExpr (LogT e b) = Log (termsToExpr e) (termsToExpr b)
termToExpr (NegT term) = Neg $ termToExpr term
termToExpr _ = error "Invalid expression given"

termsToExpr (firstTerm:terms) = prioritizedToExpr (termToExpr firstTerm) terms

toExpr terms = termsToExpr $ prioritize terms

