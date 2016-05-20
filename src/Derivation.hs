{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Derivation
  ( Derivation(..), completeTop, completeAll, getAllTypes)
where

import           Atom                (Atom (..))
import           Classes
import           Data.List           (intercalate)
import           Rule                (Rule (..))
import           Type                (Type (..))

import qualified Data.HashMap.Strict as H


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

complete :: Derivation → Int → (Derivation, Int)
complete (Node Slash τ d1 d2) st =
  let τ2 = if hasNoType d2 then Unknown st else getType d2
      st' = succ st
      τ1 = (τ :/: τ2)
      (d1', st'') = complete (changeType τ1 d1) st'
      (d2', st''') = complete (changeType τ2 d2) st''
  in (Node Slash τ d1' d2', st''')
complete (Node Backslash τ d1 d2) st =
  let τ1 = if hasNoType d1 then Unknown st else getType d1
      st' = succ st
      τ2 = (τ1 :\: τ)
      (d2', st'') = complete (changeType τ2 d2) st'
      (d1', st''') = complete (changeType τ1 d1) st''
  in  (Node Backslash τ d1' d2', st''')
complete (Leaf r w) st = (Leaf r w, st)

completeTop :: Derivation → Int → (Derivation, Int)
completeTop (Node r _ d1 d2) st =
  complete (Node r (AtomicType Sentence) d1 d2) st
completeTop (Leaf r w) st =
  complete (Leaf r w) st

-- TODO
extractConstraints :: Derivation → [(String, Type)]
extractConstraints (Node r τ d1 d2) =
  extractConstraints d1 ++ extractConstraints d2
extractConstraints (Leaf τ w) =
  [(w, τ)]

-- TODO
completeAll :: [Derivation] → [Derivation]
completeAll derivations =
  let completeAll' :: [Derivation] → Int → [Derivation]
      completeAll' [] _ = []
      completeAll' (d:ds) st =
        let (completed, st') = completeTop d st
        in completed : (completeAll' ds st')
  in completeAll' derivations 1

getAllTypes :: [Derivation] → H.HashMap String [Type]
getAllTypes ds =
  let bindings = concat . map extractConstraints $ ds
      update :: H.HashMap String [Type] → (String, Type) → H.HashMap String [Type]
      update h (s, τ) =
        case H.lookup s h of
          Just τs → H.insert s (τ : τs) h
          Nothing → H.insert s [τ] h
  in foldl update H.empty bindings

pretty' (s, τs) = s ++ " " ++ (intercalate ", " $ pretty <$> τs) ++ "\n"

instance Pretty (H.HashMap String [Type]) where

  pretty x = concat $ pretty' <$> H.toList x
