module Main where
import System.IO
import Data.HashMap.Strict as H
import System.Random as R
-- import Data.Random as R

insertBigrams :: [String] -> HashMap String [String]-> HashMap String [String]
insertBigrams[] h = h
insertBigrams (x : []) h = h
insertBigrams (x : remains@(x' : xs)) h
 | H.member x h == True = insertBigrams remains $ H.insert x (x' : (extract $ H.lookup x h)) h
 | otherwise = insertBigrams remains $ H.insert x [x'] h
 where
   extract :: Maybe [String] -> [String]
   extract (Just l) = l

printText :: (RandomGen g) => Int -> g -> String -> HashMap String [String] -> IO ()
printText 0 _ _ _ = putStr ""
printText n g k h = do
  putStr (k ++ " ")
  g <- R.newStdGen
  (printText (n-1) g nextk h)
    where
      lis = extract $ H.lookup k h
      extract (Just str) = str
      nextk = pick lis g

pick :: (RandomGen g) => [a] -> g -> a
pick xs g = xs !! fst (R.randomR (0, length xs - 1) g)

main = do
  handle <- readFile "alice30.txt"
  let line = lines handle
  let wordlist = words handle
  let bigrams = insertBigrams wordlist newMap
  putStrLn "Enter number of words to print out: "
  getAnswer <- getLine
  let num = read getAnswer :: Int
  g <- R.newStdGen
  let firstword = pick (H.keys bigrams) g
  printText num g firstword bigrams
  print num
  where
    newMap :: HashMap String [String]
    newMap = H.empty
