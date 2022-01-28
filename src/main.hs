module Main where

import Data.Char
import Data.List (isPrefixOf, isSuffixOf)
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
  | LFloat Double
  | LVariable String
  deriving (Show, Eq)

reprWord (LSymbol a) = a
reprWord (LInteger a) = show a
reprWord (LFloat a) = show a
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
  | isIntStr rawStr = LInteger (read rawStr)
  | isFloatStr rawStr = LFloat $ read rawStr
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
interpretWord state@LState {lStack = stack} word@(LFloat value) = pure $ state {lStack = word : stack}
interpretWord state@LState {lStack = stack} word@(LVariable value) = pure $ state {lStack = word : stack}
interpretWord state@LState {lStack = stack} word@(LSymbol "dup") =
  pure $ state {lStack = head stack : stack}
interpretWord state@LState {lStack = stack} word@(LSymbol "drop") =
  pure $ state {lStack = tail stack}
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
interpretWord state@LState {lStack = (LInteger a) : stack} word@(LSymbol "float") =
  pure $ state {lStack = LFloat (fromIntegral a) : stack}
interpretWord state@LState {lStack = (LFloat a) : stack} word@(LSymbol "round") =
  pure $ state {lStack = LInteger (round a) : stack}
interpretWord state@LState {lStack = stack, lDict = dict} word@(LSymbol "+") =
  let (a : b : stack') = stack
      result = lAddNumbers a b
   in pure $ state {lStack = result : stack'}
interpretWord state@LState {lStack = stack, lDict = dict} word@(LSymbol "*") =
  let (a : b : stack') = stack
      result = lMultiplyNumbers a b
   in pure $ state {lStack = result : stack'}
-- def lookup
interpretWord state@LState {lDefs = defs, lSource = source} word@(LSymbol other) =
  let defM = M.lookup other defs
   in case defM of
        Just body -> pure $ state {lSource = body ++ source}
        Nothing -> error $ "[error] not defined: " ++ other

-- error
-- interpretWord state other = error $ "[error] runtime error at " ++ show other ++ "\ninterpreter state at time of error:\n" ++ show state

lAddNumbers (LInteger a) (LInteger b) = LInteger (a + b)
lAddNumbers (LFloat a) (LFloat b) = LFloat (a + b)
lAddNumbers a b = error $ "[error] sum not defined for " ++ show a ++ ", " ++ show b

lMultiplyNumbers (LInteger a) (LInteger b) = LInteger (a * b)
lMultiplyNumbers (LFloat a) (LFloat b) = LFloat (a * b)
lMultiplyNumbers a b = error $ "[error] product not defined for " ++ show a ++ ", " ++ show b

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
