{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Classes
import           Derivation  (Derivation (..), completeAll, getAllTypes,
                              makeProblem)
import           Rule        (Rule (..))
import           Type        (Type (..))
import           Unification (unify, applyUnifier)
import Data.List (intercalate)


example1 :: Derivation
example1 = Node Backslash None
             (Node Slash None (Leaf None "a") (Leaf None "man"))
             (Leaf None "swims")

example2 :: Derivation
example2 = Node Backslash None
             (Node Slash None (Leaf None "a") (Leaf None "fish"))
             (Node Backslash None (Leaf None "swims") (Leaf None "fast"))

examples :: [Derivation]
examples = [example1, example2]

main :: IO ()
main = do
  -- let printPair (x, y) = putStrLn $ (show x) ++ " : " ++ pretty y
  let problem = makeProblem . getAllTypes . completeAll $ examples
  prettyPrint problem
  putStrLn "Solution\n--------------"
  case unify problem of
    Just unifier →
      let solution = map ((applyUnifier unifier) . fst) problem
      in putStrLn $ intercalate "\n" (map pretty solution)
    Nothing → print "Constraints do not unify."
