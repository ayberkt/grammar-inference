module Atom
  (Atom(..))
where

import Classes

data Atom
  = Sentence
  | NounPhrase
  | Noun
  | Infinitive
  deriving (Eq, Ord, Show)

instance Pretty Atom where

  pretty Sentence = "S"
  pretty NounPhrase = "NP"
  pretty Noun = "N"
  pretty Infinitive = "INF"
