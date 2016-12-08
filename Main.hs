import System.Environment
import System.Exit
import System.Random

import Control.Monad
import Control.Monad.Random

import Data.Char
import Data.HashMap.Lazy hiding (filter, map, fromList)
import Data.List

generateEdges :: [String] -> Int -> [(String,String)]
generateEdges input k
  | (length first_k == length second_k && length first_k == k) = (first, second) : generateEdges (tail input) k
  | otherwise = []
  where first_k  = take k input
        second_k = take k . drop k $ input
        first    = intercalate " " first_k
        second   = intercalate " " second_k

printEdges :: [(String,String)] -> IO ()
printEdges [] = return ()
printEdges ((a,b):xs) = do
  print $ "(" ++ a ++ "," ++ b ++ ")"
  printEdges xs


updateCounts :: Eq a => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
updateCounts x [] = x
updateCounts new@[(x,1)] old@((val,count):xs)
  | x == val  = (val,count+1):xs
  | otherwise = (val,count):(updateCounts new xs)

counts :: [(String,String)] -> HashMap String [(String,Int)]
counts [] = empty
counts ((key,value):xs) = insertWith updateCounts key [(value,1)] rest
  where rest = counts xs


totals :: HashMap String [(String,Int)] -> HashMap String Int
totals = mapWithKey (\key counts -> foldl (\acc (_,n) -> acc+n) 0 counts)

percentages :: Fractional a => HashMap String [(String,Int)] -> HashMap String [(String,a)]
percentages map1 = mapWithKey squash map1
  where totalsMap = totals map1
        squash key counts = map (\(val,freq) -> (val, fromIntegral freq / fromIntegral (totalsMap ! key))) counts


generateWords :: HashMap String [(String,Rational)] -> String -> IO ()
generateWords map seed = do
  next <- fromList $ map ! seed
  putStr $ next ++ " "
  generateWords map next

--K-gram model
k :: Int
k = 3

main = do
  args <- getArgs
  when (length args == 0) (die "Missing filename")
  --Remove punctuation, lowercase, and split into list of words
  processed <- words <$> ((fmap toLower) . filter (`notElem` ".?!-;\'\":,")) <$> readFile (args !! 0)

  --Generate probabilities of going from one k-gram to another
  let transitions = percentages $ counts $ generateEdges processed k

  --Generate the first k-gram randomly to seed the Markov generator
  seed <- uniform $ keys transitions

 --Print words ad infinitum given the seed and transition probabilities
  generateWords transitions seed
