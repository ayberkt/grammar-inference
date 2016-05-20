{-# LANGUAGE UnicodeSyntax #-}

module Derivation
  ( Derivation(..), completeTop)
where

import           Atom    (Atom (..))
import           Classes
import           Rule    (Rule (..))
import           Type    (Type (..))

data Derivation
  = Node Rule Type Derivation Derivation
  | Leaf Type String
  deriving (Eq, Show)

instance Pretty Derivation where

  pretty (Node r τ d1 d2)
    = let rs  = pretty r
          x2s = pretty d1
          x3s = pretty d2
      in " [" ++ rs ++ x2s ++ x3s ++ "]" ++ ":" ++ pretty τ ++ " "
  pretty (Leaf τ w) = " " ++ show w ++ ":" ++ pretty τ

hasNoType :: Derivation → Bool
hasNoType (Node _ None _ _) = True
hasNoType (Leaf None _)     = True
hasNoType _                 = False

getType :: Derivation → Type
getType (Node _ τ _ _) = τ
getType (Leaf τ _)     = τ

changeType :: Type → Derivation → Derivation
changeType τ (Node r _ d1 d2) = Node r τ d1 d2
changeType τ (Leaf _ w)       = Leaf τ w

complete :: Derivation → Int → Derivation
complete (Node Slash τ d1 d2) st =
  let τ2 = if hasNoType d2 then Unknown st else getType d2
      st' = succ st
      τ1 = (τ `S` τ2)
      d1' = complete (changeType τ1 d1) st'
      st'' = succ st'
      d2' = complete (changeType τ2 d2) st''
  in Node Slash τ d1' d2'
complete (Node Backslash τ d1 d2) st =
  let τ1 = if hasNoType d1 then Unknown st else getType d1
      st' = succ st
      τ2 = (τ1 `B` τ)
      d2' = complete (changeType τ2 d2) st'
      st'' = succ st
      d1' = complete (changeType τ1 d1) st''
  in  Node Backslash τ d1' d2'
complete (Leaf r w) st = Leaf r w

completeTop :: Derivation → Derivation
completeTop (Node r _ d1 d2) =
  complete (Node r (AtomicType Sentence) d1 d2) 1
completeTop (Leaf r w) =
  complete (Leaf r w) 1
