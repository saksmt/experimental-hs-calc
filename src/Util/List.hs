module Util.List(slice, replace) where

replace (s, e) v xs = take s xs ++ [v] ++ drop (e + 1) xs
slice (f, c) xs = take c $ drop f xs

