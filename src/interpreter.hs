module Interpreter where

import Control.Monad.Except
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as M
import LTypes
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin)
import Utils

debugWaitForChar Config {configDebugMode = mode} =
  if mode
    then do
      hSetBuffering stdin NoBuffering
      c <- getChar
      pure $ case c of
        'q' -> ExecExit
        _ -> ExecContinue
    else pure ExecContinue

debugPrint Config {configDebugMode = mode} message =
  if mode
    then liftIO $ putStrLn $ "[debug] " ++ message
    else pure ()

interpretSource :: Config -> LState -> ExceptT LException IO ()
interpretSource config LState {lSource = []} = pure ()
interpretSource config state@LState {lSource = (word : rest)} = do
  newState <- interpretWord state {lSource = rest} word
  liftIO $ debugPrint config $ "processed " ++ show word ++ ", newState ="
  liftIO $ debugState config newState
  step <- liftIO $ debugWaitForChar config
  case step of
    ExecContinue -> interpretSource config newState
    ExecExit -> pure ()

interpretWord :: LState -> LWord -> ExceptT LException IO LState
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
-- string literals
interpretWord state@LState {lSource = source, lStrLitRefMap = strLitRefMap} (LStringLitRef ref) =
  let strLitP = strLitRefMap ! ref
   in pure $ state {lSource = strLitP ++ source}
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
            liftIO . putStrLn $ reprWord a
            pure $ state {lStack = stack'}
          "s." -> do
            let (word@(LPhrase ws) : stack') = stack
            let isLChar w = case w of LChar _ -> True; _ -> False
            unless (all isLChar ws) $ throwError $ LException $ "cannot string-print heterogenous phrase: " ++ reprWord word
            let stringRepr = ws $> map (\(LChar c) -> c)
            liftIO . putStrLn $ stringRepr
            pure $ state {lStack = stack'}
          "?" -> do
            let (LLabel a : stack') = stack
            let lookupWord = dict ! a
            liftIO . putStrLn $ reprWord lookupWord
            pure $ state {lStack = stack'}
          "+" -> do
            let (a : b : stack') = stack
            result <- lAddNumbers b a
            pure $ state {lStack = result : stack'}
          "-" -> do
            let (a : b : stack') = stack
            result <- lSubNumbers b a
            pure $ state {lStack = result : stack'}
          "*" -> do
            let (a : b : stack') = stack
            result <- lMultiplyNumbers b a
            pure $ state {lStack = result : stack'}
          "/" -> do
            let (a : b : stack') = stack
            result <- lDivideNumbers b a
            pure $ state {lStack = result : stack'}
          "mod" -> do
            let (a : b : stack') = stack
            result <- lModNumbers b a
            pure $ state {lStack = result : stack'}
          "eq?" -> do
            let (a : b : stack') = stack
            pure $ state {lStack = LBool (a == b) : stack'}
          "gt?" -> do
            let (a : b : stack') = stack
            result <- lGreaterThan b a
            pure $ state {lStack = result : stack'}
          "lt?" -> do
            let (a : b : stack') = stack
            result <- lLesserThan b a
            pure $ state {lStack = result : stack'}
          "unphrase" ->
            let (LPhrase phrase : stack') = stack
             in pure $ state {lSource = phrase ++ source, lStack = stack'}
          "phrase" ->
            let (LSymbol "]" : stack') = stack
                (body, _ : newStack) = break (== LSymbol "[") stack'
             in pure $ state {lStack = LPhrase (reverse body) : newStack}
          "repr" ->
            let (word : stack') = stack
                reprStr = reprWord word $> map LChar .> LPhrase
             in pure $ state {lStack = reprStr : stack'}
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
          _ -> throwError $ LException $ "not defined: " ++ symbol
  | otherwise = pure $ state {lStack = word : stack}

lAddNumbers :: LWord -> LWord -> ExceptT LException IO LWord
lAddNumbers (LInteger a) (LInteger b) = pure $ LInteger (a + b)
lAddNumbers (LFloat a) (LFloat b) = pure $ LFloat (a + b)
lAddNumbers a b = throwError $ LException $ "sum is not defined for " ++ show a ++ ", " ++ show b

lSubNumbers :: LWord -> LWord -> ExceptT LException IO LWord
lSubNumbers (LInteger a) (LInteger b) = pure $ LInteger (a - b)
lSubNumbers (LFloat a) (LFloat b) = pure $ LFloat (a - b)
lSubNumbers a b = throwError $ LException $ "difference is not defined for " ++ show a ++ ", " ++ show b

lMultiplyNumbers :: LWord -> LWord -> ExceptT LException IO LWord
lMultiplyNumbers (LInteger a) (LInteger b) = pure $ LInteger (a * b)
lMultiplyNumbers (LFloat a) (LFloat b) = pure $ LFloat (a * b)
lMultiplyNumbers a b = throwError $ LException $ "product is not defined for " ++ show a ++ ", " ++ show b

lDivideNumbers :: LWord -> LWord -> ExceptT LException IO LWord
lDivideNumbers (LInteger a) (LInteger b) =
  case b of
    0 -> throwError $ LException "division by zero"
    _ -> pure $ LInteger (a `div` b)
lDivideNumbers (LFloat a) (LFloat b) =
  case b of
    0 -> throwError $ LException "division by zero"
    _ -> pure $ LFloat (a / b)
lDivideNumbers a b = throwError $ LException $ "division is not defined for " ++ show a ++ ", " ++ show b

lGreaterThan :: LWord -> LWord -> ExceptT LException IO LWord
lGreaterThan (LInteger a) (LInteger b) = pure $ LBool (a > b)
lGreaterThan (LFloat a) (LFloat b) = pure $ LBool (a > b)
lGreaterThan a b = throwError $ LException $ "greater-than is not defined for " ++ show a ++ ", " ++ show b

lLesserThan :: LWord -> LWord -> ExceptT LException IO LWord
lLesserThan (LInteger a) (LInteger b) = pure $ LBool (a < b)
lLesserThan (LFloat a) (LFloat b) = pure $ LBool (a < b)
lLesserThan a b = throwError $ LException $ "lesser-than is not defined for " ++ show a ++ ", " ++ show b

lModNumbers :: LWord -> LWord -> ExceptT LException IO LWord
lModNumbers (LInteger a) (LInteger b) = pure $ LInteger (a `mod` b)
lModNumbers a b = throwError $ LException $ "modulo is not defined for " ++ show a ++ ", " ++ show b
