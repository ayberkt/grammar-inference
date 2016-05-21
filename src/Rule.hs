module Rule where

import Classes

-- | Simple enumeration for possible rules: @Slash@ or @Backslash@.
data Rule = Slash | Backslash
          deriving (Eq, Show)

instance Pretty Rule where

  pretty Slash = "/e"
  pretty Backslash = "\\e"
