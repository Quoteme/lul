module TreeData where

data Orientation = Horizontal | Vertical
  deriving (Eq, Show)

data SplitData = SplitData
  { ratio :: Float
  , orientation :: Orientation
  }
  deriving (Eq, Show)

rotateSplit :: SplitData -> SplitData
rotateSplit (SplitData r o)
  | o == Horizontal = SplitData r Vertical
  | o == Vertical   = SplitData r Horizontal
