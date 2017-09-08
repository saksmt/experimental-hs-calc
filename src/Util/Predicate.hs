module Util.Predicate(orP, andP) where

orP :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
orP f g = \x -> f x || g x

andP :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
andP f g = \x -> f x && g x

