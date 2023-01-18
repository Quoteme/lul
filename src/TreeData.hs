module TreeData where

data Orientation = Horizontal | Vertical
  deriving (Eq, Show)

data SplitData = SplitData
  { ratio :: Float
  , orientation :: Orientation
  }
  deriving (Eq, Show)
