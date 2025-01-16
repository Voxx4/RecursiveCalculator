module DataTypes where

data BasicFunction
  = Zero
  | Successor
  | Image Int

data Function
  = Basic
    { name :: String
    , argsNum :: Int
    , basicFunc :: BasicFunction
    }
  | Superposition
    { name :: String
    , argsNum :: Int
    , outerFunc :: Function
    , argFuncs :: [Function]
    }
  | PrimitiveRecursion
    { name :: String
    , argsNum :: Int
    , baseFunc :: Function
    , recFunc :: Function
    }
  | Minimisation
    { name :: String
    , argsNum :: Int
    , toBeMinFunc :: Function
    }

data LineAST
  = SuperpositionLine
    {
      f :: String,
      g :: String,
      hs :: [String]
    }
  | MinimisationLine
    {
      f :: String,
      g :: String
    }
  | PrimitiveRecursionBase
    {
      f :: String,
      g :: String
    }
  | PrimitiveRecursionStep
    {
      f :: String,
      h :: String
    }
  deriving (Show)