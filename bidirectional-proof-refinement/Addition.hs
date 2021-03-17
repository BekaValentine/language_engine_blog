module Addition where

data Nat = Zero | Suc Nat
  deriving (Show)

data Judgment = Plus12 Nat Nat | Plus13 Nat Nat
  deriving (Show)

decomposePlus12 :: Nat -> Nat -> Maybe ([Judgment], [Nat] -> Nat)
decomposePlus12 Zero y = Just ([], \zs -> y)
decomposePlus12 (Suc x) y = Just ([Plus12 x y], \[z] -> Suc z)

decomposePlus13 :: Nat -> Nat -> Maybe ([Judgment], [Nat] -> Nat)
decomposePlus13 Zero z = Just ([], \xs -> z)
decomposePlus13 (Suc x) (Suc z) = Just ([Plus13 x z], \[x] -> x)
decomposePlus13 _ _ = Nothing

decompose :: Judgment -> Maybe ([Judgment], [Nat] -> Nat)
decompose (Plus12 x y) = decomposePlus12 x y
decompose (Plus13 x z) = decomposePlus13 x z

data ProofTree = ProofTree Judgment [ProofTree]
  deriving (Show)

findProof :: Judgment -> Maybe (ProofTree, Nat)
findProof j =
  case decompose j of
    Nothing -> Nothing
    Just (js, f) -> case sequence (map findProof js) of
      Nothing -> Nothing
      Just tns -> let (ts, ns) = unzip tns
                  in Just (ProofTree j ts, f ns)

main :: IO ()
main =
  do print (findProof (Plus12 (Suc Zero) (Suc (Suc Zero))))
     print (findProof (Plus13 (Suc Zero) Zero))