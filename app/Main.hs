module Main where

import           Classes
import           Derivation (Derivation (..), completeTop)
import           Rule       (Rule (..))
import           Type       (Type (..))


example1 :: Derivation
example1 = Node Backslash None
             (Node Slash None (Leaf None "a") (Leaf None "man"))
             (Leaf None "swims")

main :: IO ()
main = do
  prettyPrint (completeTop example1)
