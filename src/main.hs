module Main where

import Data.Char
import System.Environment (getArgs)
import Utils

getFilename [] = error "empty argument list"
getFilename (f : fs) = f

data LWord
  = LSymbol String
  | LInteger Integer
  deriving (Show)

reprWord (LSymbol a) = a
reprWord (LInteger a) = show a

parseWord rawStr
  | all isDigit rawStr = LInteger (read rawStr)
  | otherwise = LSymbol rawStr

parseSource source = source $> words .> map parseWord

interpretSource :: [LWord] -> [LWord] -> IO ()
interpretSource _ [] = putStrLn "done"
interpretSource stack (word : rest) = do
  newStack <- interpretWord stack word
  interpretSource newStack rest

interpretWord :: [LWord] -> LWord -> IO [LWord]
interpretWord stack word@(LInteger value) = pure $ word : stack
interpretWord (a : stack) word@(LSymbol ".") = do
  putStrLn $ reprWord a
  pure stack
interpretWord ((LInteger a) : (LInteger b) : stack) word@(LSymbol "+") = pure $ LInteger (a + b) : stack
interpretWord _ other = error $ "[error] runtime error at " ++ show other

main :: IO ()
main = do
  args <- getArgs
  let filename = getFilename args
  putStrLn $ "[info] executing file " ++ filename
  source <- readFile filename
  putStrLn source
  let parsed = parseSource source
  putStrLn $ "[info] parsed words:\n" ++ show parsed
  putStrLn "[info] interpreter output:"
  interpretSource [] parsed
