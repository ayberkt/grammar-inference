{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Derivation
  ( Derivation(..)
  , modifyAllTypes
  , completeTop
  , completeAll
  , getAllTypes
  , makeProblem)
where

import           Atom                (Atom (..))
import           Classes
import           Data.List           (intercalate)
import           Rule                (Rule (..))
import           Type                (Type (..))

import qualified Data.HashMap.Strict as H


-- | The @Derivation@ type represents a type derivation. The "axioms" are always
-- the words whose types come from the lexicon. At each @Node@ we mark the
-- corresponding rule as well as the @Type@. Note that the @Type@ type has a
-- constructor @None@ to represent the absence of a @Type@: these are
-- reconstructed by the @complete@ function.
data Derivation
  = Node Rule Type Derivation Derivation
  | Leaf Type String
  deriving (Eq, Show)

-- | Pretty printing for the @Derivation@ type.
instance Pretty Derivation where

  pretty (Node r τ d1 d2)
    = let rs  = pretty r
          x2s = pretty d1
          x3s = pretty d2
      in " [" ++ rs ++ x2s ++ x3s ++ "]" ++ ":" ++ pretty τ ++ " "
  pretty (Leaf τ w) = " " ++ show w ++ ":" ++ pretty τ

-- | Check if a given @Derivation@ node has no type.
hasNoType :: Derivation → Bool
hasNoType (Node _ None _ _) = True
hasNoType (Leaf None _)     = True
hasNoType _                 = False

-- | Get the type at a given @Derivation@ node (internal node @Node@ or @Leaf@
-- node).
getType :: Derivation → Type
getType (Node _ τ _ _) = τ
getType (Leaf τ _)     = τ

changeType :: Type → Derivation → Derivation
changeType τ' (Node r _ d1 d2) = Node r τ' d1 d2
changeType τ' (Leaf _ w)       = Leaf τ' w

-- | Given a function @f :: Type -> Type@, apply `f` to all types in a
-- derivation tree.
modifyAllTypes :: (Type → Type) → Derivation → Derivation
modifyAllTypes f (Node r τ d1 d2) = Node r (f τ) (modifyAllTypes f d1) (modifyAllTypes f d2)
modifyAllTypes f (Leaf τ w) = Leaf (f τ) w

-- | Given a derivation with missing type information, reconstruct the type
-- information. If the information is insufficient, use the @Unknown@
-- constructor of the @Type@ type.
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

-- | Top-level interface to the complete function. Since all sentences are
-- assumed to be correct, we fill in @S@ for the outer-most @Type@.
completeTop :: Derivation → Int → (Derivation, Int)
completeTop (Node r _ d1 d2) st =
  complete (Node r (AtomicType Sentence) d1 d2) st
completeTop (Leaf r w) st =
  complete (Leaf r w) st


-- | Get all type bindings from a given derivation.
getTypes :: Derivation → [(String, Type)]
getTypes (Node _ _ d1 d2) =
  getTypes d1 ++ getTypes d2
getTypes (Leaf τ w) =
  [(w, τ)]

completeAll :: [Derivation] → [Derivation]
completeAll derivations =
  let completeAll' :: [Derivation] → Int → [Derivation]
      completeAll' [] _ = []
      completeAll' (d:ds) st =
        let (completed, st') = completeTop d st
        in completed : completeAll' ds (succ st')
  in completeAll' derivations 1

-- | Given list of derivations @ds@, get all the type bindings from all the
-- derivations as a @HashMap String [Type]@ where each word corresponds to its
-- different types obtained from different sentences.
getAllTypes :: [Derivation] → H.HashMap String [Type]
getAllTypes ds =
  let bindings = concatMap getTypes ds
      update :: H.HashMap String [Type] → (String, Type) → H.HashMap String [Type]
      update h (s, τ) =
        case H.lookup s h of
          Just τs → H.insert s (τ : τs) h
          Nothing → H.insert s [τ] h
  in foldl update H.empty bindings

-- | Given type constraints, makes a unification problem out of them.
makeProblem :: H.HashMap String [Type] → [(Type, Type)]
makeProblem bindings =
  let makeProblem' [] _ = []
      makeProblem' (k:ks) st =
        let types =
              case H.lookup k bindings of
                Just ts → [(Unknown st, t) | t ← ts ]
                Nothing → []
        in types ++ makeProblem' ks (succ st)
  in makeProblem' (H.keys bindings) 30

instance Pretty [(Type, Type)] where

  pretty [] = ""
  pretty ((t1, t2):ts) =
    pretty t1 ++ "  =  " ++ pretty t2 ++ "\n" ++ pretty ts

instance Pretty (H.HashMap String [Type]) where

  pretty x =
      let pretty' (s, τs) =
            s ++ ": " ++ intercalate ", " (pretty <$> τs) ++ "\n"
      in concat $ pretty' <$> H.toList x


instance Pretty (H.HashMap String Type) where

  pretty x =
    let pretty' (s, τ) = s ++ ": " ++ pretty τ ++ "\n"
    in concat $ pretty' <$> H.toList x
