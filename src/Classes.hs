{-# LANGUAGE UnicodeSyntax #-}

module Classes where

class Pretty a where

  pretty :: a → String

  prettyPrint :: a → IO ()
  prettyPrint x = putStrLn (pretty x)
