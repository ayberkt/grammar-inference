module Rule where

import Classes

data Rule = Slash | Backslash
          deriving (Eq, Show)

instance Pretty Rule where

  pretty Slash = "/e"
  pretty Backslash = "\\e"
