{-# LANGUAGE UnicodeSyntax #-}

module Type
  ( Type(..)
  , isNone)
where

import Atom (Atom(..))
import Classes

data Type
  = None
    -- Type variable
  | Unknown String Int
    -- Atomic type such as n, np, or S.
  | AtomicType Atom
    -- Slash type.
  | Type :/: Type
    -- Backslash type.
  | Type :\: Type
  deriving (Eq, Ord, Show)

isNone :: Type → Bool
isNone None = True
isNone _    = False

instance Pretty Type where

  pretty (Unknown x n) = x ++ show n
  pretty None        = "None"
  pretty (AtomicType x)    = pretty x
  pretty (x :/: y)
    = "(" ++ pretty x ++ " / " ++ pretty y ++ ")"
  pretty (x :\: y)
    = "(" ++ pretty x ++ " \\ " ++ pretty y ++ ")"
