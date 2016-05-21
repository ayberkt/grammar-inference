{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Classes
-- import           Data.List   (intercalate)
import qualified Data.HashMap.Strict as H
import           Derivation          (Derivation (..), completeAll, getAllTypes,
                                      getFinalTypes, makeProblem, modifyTypes)
import           Rule                (Rule (..))
import           System.Environment  (getArgs)
import           Type                (Type (..))
import           Unification         (applyUnifier, unify)


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

learn :: [Derivation] → Maybe (H.HashMap String [Type])
learn ss =
  let completedExamples = completeAll ss
  in case unify (makeProblem . getAllTypes $ completedExamples) of
    Just unifier → Just . getFinalTypes $ map (modifyTypes (applyUnifier unifier)) completedExamples
    Nothing → Nothing

main :: IO ()
main = do
  -- let printPair (x, y) = putStrLn $ (show x) ++ " : " ++ pretty y
  args ← getArgs
  let completedExamples = completeAll examples
  if "--verbose" `elem` args
  then do
         putStrLn "I will attempt to unify the following constraints"
         putStrLn "--------------------------------------------------"
         prettyPrint $ getAllTypes completedExamples
  else return ()
  putStrLn "Solution\n--------------------------------------------------"
  case learn examples of
    Just xs → prettyPrint xs
    Nothing → putStrLn "Examples do not unify"
