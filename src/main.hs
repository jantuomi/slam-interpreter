module Main where

import Data.Char
import Data.List (isPrefixOf)
import Data.Map (Map, (!))
import qualified Data.Map as M
import System.Environment (getArgs)
import Utils

getFilename [] = error "empty argument list"
getFilename (f : fs) = f

data LWord
  = LSymbol String
  | LInteger Integer
  | LVariable String
  deriving (Show)

reprWord (LSymbol a) = a
reprWord (LInteger a) = show a
reprWord (LVariable a) = a

parseWord rawStr
  | all isDigit rawStr = LInteger (read rawStr)
  | "$" `isPrefixOf` rawStr = LVariable $ tail rawStr
  | otherwise = LSymbol rawStr

parseSource source = source $> words .> map parseWord

interpretSource _ _ [] = putStrLn "done"
interpretSource stack dict (word : rest) = do
  (newStack, newDict) <- interpretWord stack dict word
  interpretSource newStack newDict rest

-- no ops
interpretWord stack dict word@(LInteger value) = pure (word : stack, dict)
interpretWord stack dict word@(LVariable value) = pure (word : stack, dict)
-- variables & printing
interpretWord ((LVariable a) : b : stack) dict word@(LSymbol "!") =
  let newDict = M.insert a b dict
   in pure (stack, newDict)
interpretWord ((LVariable a) : stack) dict word@(LSymbol "@") =
  let lookupWord = dict ! a
   in pure (lookupWord : stack, dict)
interpretWord (a : stack) dict word@(LSymbol ".") = do
  putStrLn $ reprWord a
  pure (stack, dict)
interpretWord ((LVariable a) : stack) dict word@(LSymbol "?") = do
  let lookupWord = dict ! a
  putStrLn $ reprWord lookupWord
  pure (stack, dict)
-- math
interpretWord ((LInteger a) : (LInteger b) : stack) dict word@(LSymbol "+") = pure $ (LInteger (a + b) : stack, dict)
-- error
interpretWord stack _ other = error $ "[error] runtime error at " ++ show other ++ "\nstack at time of error:\n" ++ show stack

main :: IO ()
main = do
  args <- getArgs
  let filename = getFilename args
  putStrLn $ "[info] executing file " ++ filename
  source <- readFile filename
  putStrLn source
  let sourceWoComments = source $> lines .> filter (\line -> not ("--" `isPrefixOf` line)) .> unlines
  let parsed = parseSource sourceWoComments
  putStrLn $ "[info] parsed words:\n" ++ show parsed
  putStrLn "[info] interpreter output:"
  interpretSource [] M.empty parsed
