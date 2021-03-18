import qualified Data.Map as M

--
-- Natural number patterns and matching
--

data Nat
  = Zero
  | Suc Nat

data NatPattern
  = ZeroP
  | SucP NatPattern
  | AnyNatP
  | NatChoiceP NatPattern NatPattern
  deriving (Show)

natMatch :: NatPattern -> Nat -> Bool
natMatch ZeroP Zero
  = True
natMatch (SucP p) (Suc n)
  = natMatch p n
natMatch AnyNatP _
  = True
natMatch (NatChoiceP p p') n
  = natMatch p n || natMatch p' n


--
-- BinTree
--

data BinTree
  = Leaf
  | Branch BinTree BinTree
  deriving (Show)

data BinTreePattern
  = LeafP
  | BranchP BinTreePattern BinTreePattern
  | AnyBinTreeP
  | BinTreeChoiceP BinTreePattern BinTreePattern

binTreeMatch :: BinTreePattern -> BinTree -> Bool
binTreeMatch LeafP Leaf = True
binTreeMatch (BranchP l r) (Branch l' r')
  = binTreeMatch l l' && binTreeMatch r r'
binTreeMatch AnyBinTreeP _
  = True
binTreeMatch (BinTreeChoiceP p p') t
  = binTreeMatch p t || binTreeMatch p' t

data BinTreePatternCons
  = LeafC
  | BranchC
  | AnyBinTreeC
  deriving (Show,Eq,Ord)

type BinTreePatternC = [BinTreePatternCons]

binTreeMatchC :: BinTreePatternC -> BinTree -> Bool
binTreeMatchC p t = go p [t]
  where
    go [] [] = True
    go (LeafC:p) (Leaf:ts)
      = go p ts
    go (BranchC:p) (Branch l r:ts)
      = go p (l:r:ts)
    go (AnyBinTreeC:p) (t:ts)
      = go p ts
    go _ _ = False

{-}
data Traversal a
  = Done
  | Choice (M.Map a (Traversal a))
  deriving (Show)

binTreeMatchT :: Traversal BinTreePatternCons -> BinTree -> Bool
binTreeMatchT p t = go p [t]
  where
    go Done [] = True
    go (Choice m) (Leaf:ts)
      = goCon m LeafC ts ts
    go (Choice m) (Branch l r:ts)
      = goCon m BranchC (l:r:ts) ts
    go _ _ = False
    
    goCon m c ts ts'
      = case M.lookup c m of
          Just p | go p ts
            -> True
          _ -> case M.lookup AnyBinTreeC m of
            Just p  -> go p ts'
            Nothing -> False
-}

data Clause = Clause deriving Show

data Traversal a
  = Done Clause
  | Choice (M.Map a (Traversal a))
  deriving (Show)

binTreeMatchT :: Traversal BinTreePatternCons -> BinTree -> Maybe Clause
binTreeMatchT p t = go p [t]
  where
    go (Done c) [] = Just c
    go (Choice m) (Leaf:ts)
      = goCon m LeafC ts ts
    go (Choice m) (Branch l r:ts)
      = goCon m BranchC (l:r:ts) ts
    go _ _ = Nothing
    
    goCon m c ts ts'
      = case M.lookup c m of
          Just p | Just c <- go p ts
            -> Just c
          _ -> case M.lookup AnyBinTreeC m of
            Just p  -> go p ts'
            Nothing -> Nothing

combine :: Ord a => Traversal a -> Traversal a -> Traversal a
combine (Done c)   (Done c')   = Done c
combine (Choice m) (Choice m') = Choice (M.unionWith combine m m')

fromList :: BinTreePatternC -> Traversal BinTreePatternCons
fromList [] = Done Clause
fromList (c:cs) = Choice (M.fromList [(c, fromList cs)])

main :: IO ()
main = do print p
          print t
          print (binTreeMatchC p t)
          print p2
          print (binTreeMatchT p2 t)
          print p3
          print p4
          print p5
          print (binTreeMatchT p5 t)
  where
    p = [BranchC,AnyBinTreeC,BranchC,LeafC,AnyBinTreeC]
    t = Branch (Branch Leaf Leaf) (Branch Leaf Leaf)
    p2 = fromList p
    p3 = fromList [LeafC]
    p4 = fromList [BranchC,LeafC,LeafC]
    p5 = combine p3 p4