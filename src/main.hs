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
  | LBool Bool
  | LVariable String
  | LPhrase [LWord]
  deriving (Show, Eq)

reprWord (LSymbol a) = a
reprWord (LInteger a) = show a
reprWord (LFloat a) = show a
reprWord (LBool a) = show a
reprWord (LVariable a) = a
reprWord (LPhrase a) = "P[ " ++ unwords (map reprWord a) ++ " ]"

data LState = LState
  { lDict :: Map String LWord,
    lStack :: [LWord],
    lPhraseDepth :: Int,
    lDefs :: Map String [LWord],
    lSource :: [LWord]
  }
  deriving (Show)

evalMode state = lPhraseDepth state == 0

debugState state = do
  putStrLn $ "stack:         " ++ unwords (map reprWord (lStack state))
  putStrLn $ "source:        " ++ unwords (map reprWord (lSource state))
  putStrLn $ "defs:          " ++ unwords (M.keys (lDefs state))
  putStrLn $ "dict:          " ++ unwords (M.keys (lDict state))
  putStrLn $ "lPhraseDepth:  " ++ show (lPhraseDepth state)
  putStrLn ""

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
  -- putStrLn $ "[debug] processed " ++ show word ++ ", newState ="
  -- debugState newState
  interpretSource newState

interpretWord :: LState -> LWord -> IO LState
-- non-nestable structures
interpretWord state@LState {lStack = stack, lPhraseDepth = phraseDepth} word@(LSymbol "define") =
  pure $
    state
      { lPhraseDepth = phraseDepth + 1,
        lStack = word : stack
      }
interpretWord state@LState {lDefs = defs, lStack = stack, lPhraseDepth = phraseDepth} word@(LSymbol ";") =
  let defineSpan = takeWhile (\word -> word /= LSymbol "define") stack
      newStack = dropWhile (\word -> word /= LSymbol "define") stack $> tail
      ((LSymbol identifier) : body) = reverse defineSpan
      newDefs = M.insert identifier body defs
   in pure $
        state
          { lPhraseDepth = phraseDepth - 1,
            lDefs = newDefs,
            lStack = newStack
          }
-- words that simply move from source to stack
interpretWord state@LState {lStack = stack} word@(LInteger _) = pure $ state {lStack = word : stack}
interpretWord state@LState {lStack = stack} word@(LFloat _) = pure $ state {lStack = word : stack}
interpretWord state@LState {lStack = stack} word@(LBool _) = pure $ state {lStack = word : stack}
interpretWord state@LState {lStack = stack} word@(LVariable _) = pure $ state {lStack = word : stack}
interpretWord state@LState {lStack = stack} word@(LPhrase _) = pure $ state {lStack = word : stack}
-- phrase markers are always evaled
interpretWord state@LState {lStack = stack, lPhraseDepth = phraseDepth} word@(LSymbol "[") =
  pure $ state {lPhraseDepth = phraseDepth + 1, lStack = word : stack}
interpretWord state@LState {lStack = stack, lPhraseDepth = phraseDepth} word@(LSymbol "]") =
  let (phrase, stack') = break (== LSymbol "[") stack
      newStack = LPhrase phrase : tail stack'
   in pure $ state {lPhraseDepth = phraseDepth - 1, lStack = newStack}
-- definition lookup
interpretWord state@LState {lDefs = defs, lDict = dict, lSource = source, lPhraseDepth = phraseDepth, lStack = stack} word@(LSymbol symbol)
  | phraseDepth == 0 =
    case M.lookup symbol defs of
      Just body -> pure $ state {lSource = body ++ source}
      Nothing ->
        case symbol of
          "dup" -> pure $ state {lStack = head stack : stack}
          "drop" -> pure $ state {lStack = tail stack}
          "noop" -> pure state
          "true" -> pure $ state {lStack = LBool True : stack}
          "false" -> pure $ state {lStack = LBool False : stack}
          "not" ->
            let (LBool a : stack') = stack
             in pure $ state {lStack = LBool (not a) : stack'}
          "float" ->
            let (LInteger a : stack') = stack
             in pure $ state {lStack = LFloat (fromIntegral a) : stack'}
          "round" ->
            let (LFloat a : stack') = stack
             in pure $ state {lStack = LInteger (round a) : stack'}
          "!" ->
            let (LVariable a : b : stack') = stack
                newDict = M.insert a b dict
             in pure $ state {lDict = newDict, lStack = stack'}
          "@" ->
            let (LVariable a : stack') = stack
                lookupWord = dict ! a
             in pure $ state {lStack = lookupWord : stack'}
          "." -> do
            let (a : stack') = stack
            putStrLn $ reprWord a
            pure $ state {lStack = stack'}
          "?" -> do
            let (LVariable a : stack') = stack
            let lookupWord = dict ! a
            putStrLn $ reprWord lookupWord
            pure $ state {lStack = stack'}
          "+" ->
            let (a : b : stack') = stack
                result = lAddNumbers a b
             in pure $ state {lStack = result : stack'}
          "*" ->
            let (a : b : stack') = stack
                result = lMultiplyNumbers a b
             in pure $ state {lStack = result : stack'}
          "eq?" ->
            let (a : b : stack') = stack
             in pure $ state {lStack = LBool (a == b) : stack'}
          "cond" ->
            let (fb : tb : (LBool cond) : stack') = stack
                LPhrase branch = if cond then tb else fb
                newSource = reverse branch ++ source
             in pure $ state {lStack = stack', lSource = newSource}
          _ -> error $ "[error] not defined: " ++ symbol
  | otherwise = pure $ state {lStack = word : stack}

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
            lPhraseDepth = 0,
            lDefs = M.empty,
            lSource = parsed
          }
  interpretSource initialState
