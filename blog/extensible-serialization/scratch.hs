module Serialization where

import Control.Applicative


data TreeN = LeafN | BranchN TreeN TreeN
  deriving (Show)

serializeN :: TreeN -> [Int]
serializeN LeafN         = [0]
serializeN (BranchN l r) = serializeN l ++ serializeN r ++ [1]

deserializeN :: [Int] -> Maybe TreeN
deserializeN codes = go [] codes
  where go [t]         []       = Just t
        go stack       (0:rest) = go (LeafN:stack) rest
        go (r:l:stack) (1:rest) = go (BranchN l r:stack) rest
        go _           _        = Nothing


data TreeB = LeafB Bool | BranchB TreeB TreeB
  deriving (Show)

serializeB :: TreeB -> [Int]
serializeB (LeafB True)  = [2,0]
serializeB (LeafB False) = [3,0]
serializeB (BranchB l r) = serializeB l ++ serializeB r ++ [1]

data StackTagB = BoolTag Bool | TreeBTag TreeB

deserializeB :: [Int] -> Maybe TreeB
deserializeB codes = go [] codes
  where go [TreeBTag t] [] = Just t
        go stack (2:rest) = go (BoolTag True:stack) rest
        go stack (3:rest) = go (BoolTag False:stack) rest
        go (BoolTag b:stack) (0:rest)
          = go (TreeBTag (LeafB b):stack) rest
        go (TreeBTag r:TreeBTag l:stack) (1:rest)
          = go (TreeBTag (BranchB l r):stack) rest
        go _ _ = Nothing


data TreeI = LeafI Int | BranchI TreeI TreeI
  deriving (Show)

serializeI :: TreeI -> [Int]
serializeI (LeafI n) = [n+2,0]
serializeI (BranchI l r) = serializeI l ++ serializeI r ++ [1]

data StackTagI = IntTag Int | TreeITag TreeI
  deriving (Show)

deserializeI :: [Int] -> Maybe TreeI
deserializeI codes = go [] codes
  where go [TreeITag t] [] = Just t
        go stack (n:rest)
          | n > 1 = go (IntTag (n-2):stack) rest
        go (IntTag n:stack) (0:rest)
          = go (TreeITag (LeafI n):stack) rest
        go (TreeITag r : TreeITag l : stack)  (1:rest)
          = go (TreeITag (BranchI l r):stack) rest
        go _ _ = Nothing


data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Show)


data Parser a = Parser { runParser :: [Int] -> Maybe (a,[Int]) }

instance Functor Parser where
  fmap f ps = Parser $ \codes ->
                case runParser ps codes of
                  Nothing -> Nothing
                  Just (x, codes') -> Just (f x, codes')

instance Monad Parser where
  return x = Parser $ \codes -> Just (x,codes)
  ps >>= f = Parser $ \codes ->
               case runParser ps codes of
                 Nothing -> Nothing
                 Just (x, codes') -> runParser (f x) codes'

failed :: Parser a
failed = Parser $ \_ -> Nothing

getCode :: Parser Int
getCode = Parser $ \codes ->
            case codes of
              []     -> Nothing
              (x:xs) -> Just (x,xs)

class Serializable a where
  serialize :: a -> [Int]
  parse :: Parser a

deserialize :: Serializable a => [Int] -> Maybe a
deserialize codes = case runParser parse codes of
                      Just (x,[]) -> Just x
                      _           -> Nothing

instance Serializable Int where
  serialize n = [n]
  parse = getCode

readCode :: Int -> Parser ()
readCode c = do c' <- getCode
                if c == c'
                then return ()
                else failed

instance Serializable Bool where
  serialize True = [0]
  serialize False = [1]
  parse = readCode 0 *> return True
      <|> readCode 1 *> return False

instance Serializable a => Serializable (Tree a) where
  serialize (Leaf x) = [0] ++ serialize x
  serialize (Branch l r) = [1] ++ serialize l ++ serialize r
  parse = readCode 0 *> (Leaf <$> parse)
      <|> readCode 1 *> (Branch <$> parse <*> parse)

instance Applicative Parser where
  pure x = Parser $ \codes -> Just (x,codes)
  af <*> ax = Parser $ \codes ->
                case runParser af codes of
                  Nothing -> Nothing
                  Just (f,codes') -> case runParser ax codes' of
                    Nothing -> Nothing
                    Just (x,codes'') -> Just (f x, codes'')

instance Alternative Parser where
  empty = failed
  p <|> p' = Parser $ \codes ->
               case runParser p codes of
                 Nothing -> runParser p' codes
                 Just r  -> Just r

main :: IO ()
main = do
  print $ serialize True
  print $ serialize False
  print $ serialize (5::Int)
  print $ serialize (Branch (Leaf True) (Leaf False))
  print $ (deserialize [0] :: Maybe Bool)
  print $ (deserialize [1] :: Maybe Bool)
  print $ (deserialize [5] :: Maybe Int)
  print $ (deserialize [1,0,0,0,1] :: Maybe (Tree Bool))