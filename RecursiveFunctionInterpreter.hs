{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

import System.IO 
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Char
import Data.Either
import DataTypes
import ParsingUtils

-- f = g(h1, h2... hn)

-- f 0 = g
-- f n+1 = h

-- f = n[g]

runBaseFunc :: BasicFunction -> [Int] -> Int
runBaseFunc Zero (_:_) = 0
runBaseFunc Successor (x:_) = x + 1
runBaseFunc (Image 1) (x:_) = x
runBaseFunc (Image n) (_:xs)
  | n <= length xs + 1 = runBaseFunc (Image (n-1)) xs
  | otherwise = error $ "Index out of bounds: arg len is: " ++ show n
runBaseFunc _ [] = error "Not enough arguments"

calculate :: Function -> [Int] -> Int
calculate _ [] = error "Not arguments given"
calculate (Basic name n f) args
  | n /= length args = wrongNumArgsError name n (length args)
  | otherwise = runBaseFunc f args
calculate (Superposition name n g hs) args
  | n /= length args = wrongNumArgsError name n (length args)
  | otherwise = calculate g [ calculate h args | h <- hs]
calculate f@(PrimitiveRecursion name n g h) args@(x1:xs)
  | n /= length args = wrongNumArgsError name n (length args)
  | x1 == 0 = calculate g xs
  | otherwise = calculate h (args ++ [calculate f ((x1 - 1):xs)])
calculate f@(Minimisation name n h) args
  | n /= length args = wrongNumArgsError name n (length args)
  | calculate h args == 0 = last args
  | otherwise = calculate f (init args ++ [last args + 1])

wrongNumArgsError :: String -> Int -> Int -> a
wrongNumArgsError name n actual = error $ "Wrong number of arguments for " ++ show name ++ ": expected " ++ show n ++ ", got " ++ show actual 

getArgsNum :: Function -> Int
getArgsNum (Basic _ n _) = n
getArgsNum (Superposition _ n _ _) = n
getArgsNum (PrimitiveRecursion _ n _ _) = n
getArgsNum (Minimisation _ n _) = n

getFuncs :: [Function] -> [String] -> [Function]
getFuncs fs = map (\ x -> head (filter (\ f -> name f == x) fs))

getFuncByName :: [Function] -> String -> Function
getFuncByName fs name = head (getFuncs fs [name])

basicFuncs :: [Function]
basicFuncs =
  [ Basic "0" 1 Zero
  , Basic "S" 1 Successor
  , Basic "I11" 1 (Image 1)
  , Basic "I12" 2 (Image 1)
  , Basic "I22" 2 (Image 2)
  , Basic "I13" 3 (Image 1)
  , Basic "I23" 3 (Image 2)
  , Basic "I33" 3 (Image 3)
  , Basic "I22" 4 (Image 4)
  , Basic "I13" 4 (Image 1)
  , Basic "I23" 4 (Image 2)
  , Basic "I33" 4 (Image 3)
  ]

transformFunctions :: [LineAST] -> [Function] -> [Function]
transformFunctions [] fs = fs
transformFunctions ((SuperpositionLine f g hs):xs) fs
  | let argumentFuncs = getFuncs fs hs
        target = getArgsNum (head argumentFuncs)
    in not (all (\ func -> getArgsNum func == target) argumentFuncs) = error ("Inner functions have different number of arguments in the definition of " ++ show f)
  | not (any (\ func -> name func == g) fs) = error ("Outer function " ++ show g ++ " not defined")
  | not (all (\ h -> any (\ func -> name func == h) fs) hs) = error ("Some inner function of " ++ show f ++ " is not defined")
  | otherwise =
    let argumentFuncs = getFuncs fs hs
        outer = getFuncByName fs g
        result = Superposition f (getArgsNum (head argumentFuncs)) outer argumentFuncs
    in transformFunctions xs (result:fs)

transformFunctions ((MinimisationLine f g):xs) fs
  | not (any (\ func -> name func == g) fs) = error ("Function " ++ show g ++ " not defined")
  | otherwise =
    let toBeMinFunc = getFuncByName fs g
        result = Minimisation f (1 - getArgsNum toBeMinFunc) toBeMinFunc
    in transformFunctions xs (result:fs)

transformFunctions ((PrimitiveRecursionBase name1 g):(PrimitiveRecursionStep name2 h):xs) fs
  | name1 /= name2 = error ("Base and step definition have different names: " ++ show name1 ++ " and " ++ show name2)
  | not (any (\ f -> name f == g) fs) = error ("Base function " ++ show g ++ " not defined")
  | not (any (\ f -> name f == h) fs) = error ("Step function " ++ show h ++ " not defined")
  | let baseF = getFuncByName fs g
        stepF = getFuncByName fs h
    in getArgsNum baseF + 2 /= getArgsNum stepF = error ("Base function " ++ show g ++ " and step function " ++ show h ++ " have incompatible number of arguments")
  | otherwise = 
     let baseF = getFuncByName fs g
         stepF = getFuncByName fs h
         result = PrimitiveRecursion name1 (getArgsNum baseF + 1) baseF stepF
     in transformFunctions xs (result:fs)
transformFunctions ((PrimitiveRecursionBase f _):_) _ = error ("No step function found for " ++ show f)
transformFunctions ((PrimitiveRecursionStep f _):_) _ = error ("No base function found for " ++ show f)

-- toshko

loop :: [Function] -> IO [Function]
loop fs = do
  line <- getLine
  if line == "#"
    then return []
    else do
      let funcName = takeWhile (/= ' ') line
      putStrLn $ "First word: " ++ firstWord

stringToNum :: String -> Int -> Int
stringToNum [] n = n
stringToNum (x:xs) n = stringToNum xs ((n*10) + (digitToInt x))


--parseArgs :: Parser [Int]
--parseArgs = do
  --_ <- char '('
  --argument <- parseNum
  --return argument


load :: IO [Function]
load = do
  file <- readFile' "basicFunctions.txt"
  let linesAST = fromRight (error "Parse error") $ parse parseFile "" file
  --print $ errorBundlePretty linesAST
  return (transformFunctions linesAST basicFuncs)

--takeRights :: [Either a b] -> [b]
--takeRights = map fst . takeWhile snd $ zip xs (True : map isRight xs)
  --where isRight (Right _) = True
        --isRight _         = False

main :: IO ()
main = do
  functions <- load
  --print "hi"
  loop functions
  print (calculate (head functions) [30, 3])
  --let functions = transformFunctions [line] basicFuncs
  --let result = (calculate (head functions) [30, 3])
  --print result 
  --let functions = transformFunctions [line] basicFuncs
  --print (calculate (head functions) [30, 3])
  --let linesAST = [SuperpositionLine "h" "S" ["I33"], PrimitiveRecursionBase "f" "I11", PrimitiveRecursionStep "f" "h"]
  --let testFuncs = [Base "g" 2 (Image 1), Base "h1" 2 (Image 1), Base "h2" 2 (Image 2)]
  --let functions = transformFunctions linesAST basicFuncs
  --print (calculate (head functions) [30, 3])
  --print (calculate (head functions) [30, 3])


