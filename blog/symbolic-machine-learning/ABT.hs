module ABT where

import Data.List (elemIndex)





data Variable = Free String | Bound String Int
  deriving (Show)



data ABT = Var Variable
         | Con String [Scope]
  deriving (Show)



data Scope = Scope [String] ABT
  deriving (Show)



bind :: Int -> [String] -> ABT -> ABT
bind _ [] x = x
bind l ns (Var v@(Free n)) =
  case elemIndex n ns of
    Nothing -> Var v
    Just i -> Var (Bound n (l + i))
bind _ _ (Var v) = Var v
bind l ns (Con c xs) = Con c (map (bindScope l ns) xs)



bindScope :: Int -> [String] -> Scope -> Scope
bindScope _ [] sc = sc
bindScope l ns (Scope ns' b) =
  Scope ns' (bind (l + length ns') ns b)



scope :: [String] -> ABT -> Scope
scope ns b = Scope ns (bind 0 ns b)