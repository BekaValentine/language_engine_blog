{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import Control.Applicative ((<$>),(<*))
import Data.List (nub)
import Text.ParserCombinators.Parsec

data Exp = Lit Integer | Plus Exp Exp | Minus Exp Exp | Times Exp Exp
  deriving (Show)

data Loc = Root
         | PlusL | PlusR
         | MinusL | MinusR
         | TimesL | TimesR
  deriving (Eq)

class Eq l => Pretty a l | a -> l where
  prettyAux :: a -> ([l], String)

prettyLoc :: Pretty a l => Maybe l -> a -> String
prettyLoc Nothing e
  = snd (prettyAux e)
prettyLoc (Just loc) e
  = let (locs, str) = prettyAux e
    in if loc `elem` locs
       then str
       else "(" ++ str ++ ")"

prettyPrint :: Pretty a l => a -> String
prettyPrint = prettyLoc Nothing


instance Pretty Exp Loc where
  prettyAux (Lit n)
    = ( [PlusL,PlusR,MinusL,MinusR,TimesL,TimesR]
      , show n
      )
  prettyAux (Plus l r)
    = ( [PlusR,MinusL]
      , prettyLoc (Just PlusL) l ++ " + " ++ prettyLoc (Just PlusR) r
      )
  prettyAux (Minus l r)
    = ( [PlusL,MinusL]
      , prettyLoc (Just MinusL) l ++ " - " ++ prettyLoc (Just MinusR) r
      )
  prettyAux (Times l r)
    = ( [PlusL,PlusR,MinusL,MinusR,TimesL]
      , prettyLoc (Just TimesL) l ++ " * " ++ prettyLoc (Just TimesR) r
      )


symbol_ :: String -> GenParser Char st ()
symbol_ s = do _ <- string s <* spaces
               return ()

tryOrElse :: [GenParser a st b] -> GenParser a st b -> GenParser a st b
tryOrElse []     e = e
tryOrElse (p:ps) e = try p <|> tryOrElse ps e

op :: (Exp -> Exp -> Exp)
   -> String
   -> GenParser Char st Exp
   -> GenParser Char st Exp
   -> GenParser Char st Exp
op f s lp rp = do l <- lp
                  symbol_ s
                  r <- rp
                  return (f l r)

-- each kind of expression
literal = Lit . read <$> many1 digit
plus  = op Plus "+" plusL plusR
minus = op Minus "-" minusL minusR
times = op Times "*" timesL timesR

-- and each kind of location
plusL  = tryOrElse [parenthesized,minus] literal
plusR  = tryOrElse [parenthesized,plus] literal
minusL = tryOrElse [parenthesized,plus,minus,times] literal
minusR = tryOrElse [parenthesized,times] literal
timesL = tryOrElse [parenthesized,times] literal
timesR = try parenthesized <|> literal

parenthesized = between (symbol_ "(") (symbol_ ")") expression
expression = tryOrElse [parenthesized,plus,minus,times] literal

main :: IO ()
main = do print (length exps)
          print (length (nub (map prettyPrint exps)))
          putStrLn $ unlines (map prettyPrint exps)
          print $ parse expression "(unknown)" "(0 + 0) - 0"
  where
    i = Lit 0
    exps = [ Plus (Plus i i) i
           , Plus i (Plus i i)
           , Plus (Minus i i) i
           , Plus i (Minus i i)
           , Plus (Times i i) i
           , Plus i (Times i i)
           , Minus (Plus i i) i
           , Minus i (Plus i i)
           , Minus (Minus i i) i
           , Minus i (Minus i i)
           , Minus (Times i i) i
           , Minus i (Times i i)
           , Times (Plus i i) i
           , Times i (Plus i i)
           , Times (Minus i i) i
           , Times i (Minus i i)
           , Times (Times i i) i
           , Times i (Times i i)
           ]