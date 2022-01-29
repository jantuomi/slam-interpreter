module Main where

import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as M
import Debug.Trace (trace, traceShow)
import LTypes
import Parser
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin)
import Utils

data Config = Config
  { configFileNameM :: Maybe String,
    configDebugMode :: Bool
  }

getConfig config [] = config
getConfig config ("--debug" : rest) =
  let newConfig = config {configDebugMode = True}
   in getConfig newConfig rest
getConfig config (fileName : rest) =
  let newConfig = config {configFileNameM = Just fileName}
   in getConfig newConfig rest

debugPrint Config {configDebugMode = mode} message =
  if mode
    then putStrLn $ "[debug] " ++ message
    else pure ()

data LState = LState
  { lDict :: Map String LWord,
    lStack :: [LWord],
    lPhraseDepth :: Int,
    lDefs :: Map String [LWord],
    lSource :: [LWord],
    lStrLitRefMap :: Map String String
  }
  deriving (Show)

evalMode state = lPhraseDepth state == 0

debugState Config {configDebugMode = mode} state =
  if mode
    then do
      putStrLn $ "stack:         " ++ unwords (map reprWord (lStack state))
      putStrLn $ "source:        " ++ unwords (map reprWord (lSource state))
      putStrLn $ "defs:          " ++ unwords (M.keys (lDefs state))
      putStrLn $ "dict:          " ++ unwords (M.keys (lDict state))
      putStrLn $ "lPhraseDepth:  " ++ show (lPhraseDepth state)
      putStrLn ""
    else pure ()

data ExecStep = ExecContinue | ExecExit

debugWaitForChar Config {configDebugMode = mode} =
  if mode
    then do
      hSetBuffering stdin NoBuffering
      c <- getChar
      pure $ case c of
        'q' -> ExecExit
        _ -> ExecContinue
    else pure ExecContinue

interpretSource :: Config -> LState -> IO ()
interpretSource config LState {lSource = []} = pure ()
interpretSource config state@LState {lSource = (word : rest)} = do
  newState <- interpretWord state {lSource = rest} word
  debugPrint config $ "processed " ++ show word ++ ", newState ="
  debugState config newState
  step <- debugWaitForChar config
  case step of
    ExecContinue -> interpretSource config newState
    ExecExit -> pure ()

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
interpretWord state@LState {lStack = stack} word@(LChar _) = pure $ state {lStack = word : stack}
interpretWord state@LState {lStack = stack} word@(LLabel _) = pure $ state {lStack = word : stack}
interpretWord state@LState {lStack = stack} word@(LPhrase _) = pure $ state {lStack = word : stack}
interpretWord state@LState {lStack = stack, lStrLitRefMap = strLitRefMap} (LStringLitRef ref) =
  let strLit = strLitRefMap ! ref
      chars = strLit $> map LChar
      word = LPhrase chars
   in pure $ state {lStack = word : stack}
-- phrase markers are always evaled
interpretWord state@LState {lStack = stack, lPhraseDepth = phraseDepth} word@(LSymbol "[") =
  pure $ state {lPhraseDepth = phraseDepth + 1, lStack = word : stack}
interpretWord state@LState {lStack = stack, lPhraseDepth = phraseDepth} word@(LSymbol "]") =
  let (phrase, stack') = break (== LSymbol "[") stack
      newStack = LPhrase (reverse phrase) : tail stack'
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
          "clear" -> pure $ state {lStack = []}
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
            let (LLabel a : b : stack') = stack
                newDict = M.insert a b dict
             in pure $ state {lDict = newDict, lStack = stack'}
          "@" ->
            let (LLabel a : stack') = stack
                lookupWord = dict ! a
             in pure $ state {lStack = lookupWord : stack'}
          "forget" ->
            let (LLabel a : stack') = stack
                newDict = M.delete a dict
             in pure $ state {lStack = stack', lDict = newDict}
          "." -> do
            let (a : stack') = stack
            putStrLn $ reprWord a
            pure $ state {lStack = stack'}
          "s." -> do
            let (word@(LPhrase ws) : stack') = stack
            let isLChar w = case w of LChar _ -> True; _ -> False
            if not (all isLChar ws)
              then error $ "[error] cannot string-print heterogenous phrase: " ++ reprWord word
              else pure ()
            let stringRepr = ws $> map (\(LChar c) -> c)
            putStrLn stringRepr
            pure $ state {lStack = stack'}
          "?" -> do
            let (LLabel a : stack') = stack
            let lookupWord = dict ! a
            putStrLn $ reprWord lookupWord
            pure $ state {lStack = stack'}
          "+" ->
            let (a : b : stack') = stack
                result = lAddNumbers b a
             in pure $ state {lStack = result : stack'}
          "-" ->
            let (a : b : stack') = stack
                result = lSubNumbers b a
             in pure $ state {lStack = result : stack'}
          "*" ->
            let (a : b : stack') = stack
                result = lMultiplyNumbers b a
             in pure $ state {lStack = result : stack'}
          "/" ->
            let (a : b : stack') = stack
                result = lDivideNumbers b a
             in pure $ state {lStack = result : stack'}
          "mod" ->
            let (a : b : stack') = stack
                result = lModNumbers b a
             in pure $ state {lStack = result : stack'}
          "eq?" ->
            let (a : b : stack') = stack
             in pure $ state {lStack = LBool (a == b) : stack'}
          "gt?" ->
            let (a : b : stack') = stack
             in pure $ state {lStack = lGreaterThan b a : stack'}
          "lt?" ->
            let (a : b : stack') = stack
             in pure $ state {lStack = lLesserThan b a : stack'}
          "unphrase" ->
            let (LPhrase phrase : stack') = stack
             in pure $ state {lSource = phrase ++ source, lStack = stack'}
          "phrase" ->
            let (LSymbol "]" : stack') = stack
                (body, _ : newStack) = break (== LSymbol "[") stack'
             in pure $ state {lStack = LPhrase (reverse body) : newStack}
          "pop" ->
            let (LPhrase phrase : stack') = stack
                (first : rest) = phrase
             in pure $ state {lStack = first : LPhrase rest : stack'}
          "stack-size" ->
            let size = LInteger $ fromIntegral (length stack)
             in pure $ state {lStack = size : stack}
          "']" ->
            pure $ state {lStack = LSymbol "]" : stack}
          "'[" ->
            pure $ state {lStack = LSymbol "[" : stack}
          "cond" ->
            let (fb : tb : (LBool cond) : stack') = stack
                LPhrase branch = if cond then tb else fb
                newSource = branch ++ source
             in pure $ state {lStack = stack', lSource = newSource}
          "loop" ->
            let (bodyP@(LPhrase body) : condP@(LPhrase cond) : stack') = stack
                ifWords = cond ++ [LPhrase (body ++ [condP, bodyP, LSymbol "loop"])] ++ [LPhrase [], LSymbol "cond"]
                newSource = ifWords ++ source
             in pure $ state {lStack = stack', lSource = newSource}
          _ -> error $ "[error] not defined: " ++ symbol
  | otherwise = pure $ state {lStack = word : stack}

lAddNumbers (LInteger a) (LInteger b) = LInteger (a + b)
lAddNumbers (LFloat a) (LFloat b) = LFloat (a + b)
lAddNumbers a b = error $ "[error] sum is not defined for " ++ show a ++ ", " ++ show b

lSubNumbers (LInteger a) (LInteger b) = LInteger (a - b)
lSubNumbers (LFloat a) (LFloat b) = LFloat (a - b)
lSubNumbers a b = error $ "[error] difference is not defined for " ++ show a ++ ", " ++ show b

lMultiplyNumbers (LInteger a) (LInteger b) = LInteger (a * b)
lMultiplyNumbers (LFloat a) (LFloat b) = LFloat (a * b)
lMultiplyNumbers a b = error $ "[error] product is not defined for " ++ show a ++ ", " ++ show b

lDivideNumbers (LInteger a) (LInteger b) = LInteger (a `div` b)
lDivideNumbers (LFloat a) (LFloat b) = LFloat (a / b)
lDivideNumbers a b = error $ "[error] product is not defined for " ++ show a ++ ", " ++ show b

lGreaterThan (LInteger a) (LInteger b) = LBool (a > b)
lGreaterThan (LFloat a) (LFloat b) = LBool (a > b)
lGreaterThan a b = error $ "[error] greater-than is not defined for " ++ show a ++ ", " ++ show b

lLesserThan (LInteger a) (LInteger b) = LBool (a < b)
lLesserThan (LFloat a) (LFloat b) = LBool (a < b)
lLesserThan a b = error $ "[error] lesser-than is not defined for " ++ show a ++ ", " ++ show b

lModNumbers (LInteger a) (LInteger b) = LInteger (a `mod` b)
lModNumbers a b = error $ "[error] modulo is not defined for " ++ show a ++ ", " ++ show b

main :: IO ()
main = do
  args <- getArgs
  let initialConfig =
        Config
          { configFileNameM = Nothing,
            configDebugMode = False
          }
  let config = getConfig initialConfig args
  let fileName = case configFileNameM config of
        Just x -> x
        Nothing -> error "[error] no file name specified"

  debugPrint config $ "executing file " ++ fileName ++ "\n"
  source <- readFile fileName

  let (parsedSource, strLitRefMap) = parseSource source
  debugPrint config $ "parsed words:\n" ++ show parsedSource ++ "\n"
  debugPrint config $ "string literal refmap:\n" ++ show strLitRefMap ++ "\n"
  debugPrint config "interpreter output:"
  let initialState =
        LState
          { lDict = M.empty,
            lStack = [],
            lPhraseDepth = 0,
            lDefs = M.empty,
            lSource = parsedSource,
            lStrLitRefMap = strLitRefMap
          }
  interpretSource config initialState
