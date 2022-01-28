module Main where

import Data.Char
import Data.List (isPrefixOf)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Debug.Trace (trace, traceShow)
import System.Environment (getArgs)
import Utils

getFilename [] = error "empty argument list"
getFilename (f : fs) = f

data LWord
  = LSymbol String
  | LInteger Integer
  | LVariable String
  deriving (Show, Eq)

reprWord (LSymbol a) = a
reprWord (LInteger a) = show a
reprWord (LVariable a) = a

data LState = LState
  { lDict :: Map String LWord,
    lStack :: [LWord],
    lEvalMode :: Bool,
    lDefs :: Map String [LWord],
    lSource :: [LWord]
  }
  deriving (Show)

parseWord rawStr
  | all isDigit rawStr = LInteger (read rawStr)
  | "$" `isPrefixOf` rawStr = LVariable $ tail rawStr
  | otherwise = LSymbol rawStr

parseSource source = source $> words .> map parseWord

interpretSource :: LState -> IO ()
interpretSource LState {lSource = []} = putStrLn "done"
interpretSource state@LState {lSource = (word : rest)} = do
  newState <- interpretWord state {lSource = rest} word
  -- putStrLn $ "[debug] processed " ++ show word ++ ", newState = " ++ show newState
  interpretSource newState

-- basics
interpretWord :: LState -> LWord -> IO LState
interpretWord state@LState {lStack = stack} word@(LSymbol "define") =
  pure $
    state
      { lEvalMode = False,
        lStack = word : stack
      }
interpretWord state@LState {lDefs = defs, lStack = stack} word@(LSymbol ";") =
  let defineSpan = takeWhile (\word -> word /= LSymbol "define") stack
      newStack = dropWhile (\word -> word /= LSymbol "define") stack $> tail
      ((LSymbol identifier) : body) = reverse defineSpan
      newDefs = M.insert identifier body defs
   in pure $
        state
          { lEvalMode = True,
            lDefs = newDefs,
            lStack = newStack
          }
interpretWord state@LState {lStack = stack, lEvalMode = False} word = pure $ state {lStack = word : stack}
interpretWord state@LState {lStack = stack} word@(LInteger value) = pure $ state {lStack = word : stack}
interpretWord state@LState {lStack = stack} word@(LVariable value) = pure $ state {lStack = word : stack}
interpretWord state@LState {lStack = stack} word@(LSymbol "dup") =
  let (stackHead : stack') = stack
   in pure $ state {lStack = stackHead : stackHead : stack}
-- variables & printing
interpretWord state@LState {lStack = (LVariable a) : b : stack, lDict = dict} word@(LSymbol "!") =
  let newDict = M.insert a b dict
   in pure $ state {lDict = newDict, lStack = stack}
interpretWord state@LState {lStack = (LVariable a) : stack, lDict = dict} word@(LSymbol "@") =
  let lookupWord = dict ! a
   in pure $ state {lStack = lookupWord : stack}
interpretWord state@LState {lStack = a : stack} word@(LSymbol ".") = do
  putStrLn $ reprWord a
  pure $ state {lStack = stack}
interpretWord state@LState {lStack = ((LVariable a) : stack), lDict = dict} word@(LSymbol "?") = do
  let lookupWord = dict ! a
  putStrLn $ reprWord lookupWord
  pure $ state {lStack = stack}
-- math
interpretWord state@LState {lStack = stack, lDict = dict} word@(LSymbol "+") =
  let ((LInteger a) : (LInteger b) : stack') = stack
   in pure $ state {lStack = LInteger (a + b) : stack'}
-- def lookup
interpretWord state@LState {lDefs = defs, lSource = source} word@(LSymbol other) =
  let defM = M.lookup other defs
   in case defM of
        Just body -> pure $ state {lSource = body ++ source}
        Nothing -> error $ "[error] not defined: " ++ other

-- error
-- interpretWord state other = error $ "[error] runtime error at " ++ show other ++ "\ninterpreter state at time of error:\n" ++ show state

main :: IO ()
main = do
  args <- getArgs
  let filename = getFilename args
  putStrLn $ "[info] executing file " ++ filename
  source <- readFile filename
  -- putStrLn source
  let sourceWoComments = source $> lines .> filter (\line -> not ("--" `isPrefixOf` line)) .> unlines
  let parsed = parseSource sourceWoComments
  -- putStrLn $ "[info] parsed words:\n" ++ show parsed
  putStrLn "[info] interpreter output:"
  let initialState =
        LState
          { lDict = M.empty,
            lStack = [],
            lEvalMode = True,
            lDefs = M.empty,
            lSource = parsed
          }
  interpretSource initialState
