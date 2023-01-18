module Path where

data Direction = Left | Right
  deriving (Eq, Show)

data Path = Path [Direction]
  deriving (Eq, Show)

instance Semigroup Path where
  (<>) (Path []) p = p
  (<>) p (Path []) = p
  (<>) (Path p1) (Path p2) = Path (p1 <> p2)

instance Monoid Path where
  mempty = Path []
