{-# LANGUAGE UnicodeSyntax #-}

module Unification where

import Type (Type(..))
import Data.HashMap.Strict as H

type Problem = [(Type, Type)]
type Unifier = H.HashMap Int Type

-- Substitute n for x in e.
subst :: Int → Type → Type → Type
subst _ _ None = error "At this point, no types should be missing."
subst n x (Unknown k)
  = if k == n then x else Unknown k
subst _ _ (AtomicType a) = AtomicType a
subst n x (τ1 :/: τ2) = (subst n x τ1) :/: (subst n x τ2)
subst n x (τ1 :\: τ2) = subst n x τ1 :\: subst n x τ2

occurs :: Type → Type → Bool
occurs   (Unknown n) (Unknown k) = n == k
occurs x@(Unknown _) (a :/: b)   = occurs x a || occurs x b
occurs x@(Unknown _) (a :\: b)   = occurs x a || occurs x b
occurs   (Unknown _) None
  = error "at this point, no types can be missing."
occurs   (Unknown _) _           = False
occurs _ _ = error "left-hand side must be a variable."

applyUnifier :: Unifier → Type → Type
applyUnifier σ x =
  let appSubst [] y = y
      appSubst ((a, b):ss) y = appSubst ss (subst a b y)
  in appSubst (H.toList σ) x

unify :: Problem → Maybe Unifier
unify [] = Just H.empty
unify (((s1 :/: s2), (t1 :/: t2)) : ss) =
  unify $ (s1, t1) : (s2, t2) : ss
unify (((s1 :\: s2), (t1 :\: t2)) : ss) =
  unify $ (s1, t1) : (s2, t2) : ss
unify ((s, t):ss) =
  if s == t
  then unify ss
  else case s of
         Unknown x → eliminate (Unknown x) t ss
         _ → case t of
               Unknown y → unify $ ((Unknown y), s) : ss
               _ → Nothing

eliminate :: Type → Type → Problem → Maybe Unifier
eliminate (Unknown x) t ss =
  if occurs (Unknown x) t
  then Nothing
  else
    let xt = applyUnifier (H.fromList [(x, t)])
        ss' = (\(t1, t2) → (xt t1, xt t2)) <$> ss
    in case unify ss' of
      Just σ' →
        let xt' = applyUnifier σ'
        in Just $ H.insert x (xt' t) σ'
      Nothing → Nothing
eliminate _ _ _ = Nothing
