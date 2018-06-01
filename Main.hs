module Main where
import System.IO
import Data.HashMap.Strict as H
import Data.List
import System.Random as R

insertBigrams :: [String] -> HashMap String [String]-> HashMap String [String]
insertBigrams [] h = h
insertBigrams (x : []) h = h
insertBigrams (x : rest@(x' : xs)) h = insertBigrams rest $ H.insertWith f x [x'] h
   where f a b = a ++ b

printText :: (RandomGen g) => Int -> g -> String -> HashMap String [String] -> IO ()
printText 0 _ _ _ = putStr "\n"
printText n g k h = case H.lookup k h of
  Nothing -> error "invalid input"
  Just v -> do
             putStr (k ++ " ")
             printText (n-1) nextG nextK h
                where (nextK, nextG) = pick v g

pick :: (RandomGen g) => [a] -> g -> (a,g)
pick xs g = (xs !! a, ng)
   where (a,ng) = R.randomR (0, length xs - 1) g

main = do
  handle <- readFile "alice30.txt"
  let wordlist = concatMap (\x -> words x) $ lines handle
  let bigrams = insertBigrams wordlist H.empty
  putStrLn "Enter number of words to print out: "
  getAnswer <- getLine
  let num = read getAnswer :: Int
  g <- R.newStdGen
  let (firstword,_) = pick (H.keys bigrams) g
  printText num g firstword bigrams
