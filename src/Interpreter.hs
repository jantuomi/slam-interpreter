module Interpreter where

import Control.Monad.Except
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as M
import LTypes
import Utils

-- | Wait for unbuffered input (any key) from stdin. If 'q', return 'ExecExit', otherwise 'ExecContinue'.
-- If not in debug mode, immediately return 'ExecContinue' without waiting.
debugWaitForChar Config {configDebugMode = mode} = do
  if mode
    then do
      c <- getChar
      pure $ case c of
        'q' -> ExecExit
        _ -> ExecContinue
    else pure ExecContinue

-- | If in debug mode, print the supplied message, automatically prefixed with "[debug]". Otherwise, no op.
debugPrint Config {configDebugMode = mode} message =
  if mode
    then liftIO $ putStrLn $ fmt "[debug] %%" [message]
    else pure ()

-- |
interpretSource :: Config -> LState -> ExceptT LException IO LState
interpretSource config state@LState {lSource = []} = pure state
interpretSource config state@LState {lSource = (word : rest)} = do
  liftIO $ debugPrint config $ fmt "processing %%, current state =" [show word]
  liftIO $ debugState config state
  newState <- interpretWord state {lSource = rest} word
  step <- liftIO $ debugWaitForChar config
  case step of
    ExecContinue -> interpretSource config newState
    ExecExit -> pure newState

interpretWord :: LState -> LWord -> ExceptT LException IO LState
-- non-nestable structures
interpretWord state@LState {lStack = stack, lPhraseDepth = phraseDepth} word@(LSymbol "define") =
  pure $
    state
      { lPhraseDepth = phraseDepth + 1,
        lStack = word : stack
      }
interpretWord state@LState {lDefs = defs, lStack = stack, lPhraseDepth = phraseDepth} word@(LSymbol ";") = do
  let defineSpan = takeWhile (\word -> word /= LSymbol "define") stack
  let newStack = dropWhile (\word -> word /= LSymbol "define") stack $> tail
  (LSymbol identifier, body) <- consume1 LSymbolT (reverse defineSpan)
  let newDefs = M.insert identifier body defs
  pure $
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
interpretWord state@LState {lStack = stack, lPhraseDepth = phraseDepth} word@(LSymbol "]") = do
  (phrase, _ : stack') <- safeBreak (== LSymbol "[") (LException "phrase-start marker '[' missing in stack") stack
  let newStack = LPhrase (reverse phrase) : stack'
  pure $ state {lPhraseDepth = phraseDepth - 1, lStack = newStack}
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
          "not" -> do
            (LBool a, stack') <- consume1 LBoolT stack
            pure $ state {lStack = LBool (not a) : stack'}
          "float" -> do
            (LInteger a, stack') <- consume1 LIntegerT stack
            pure $ state {lStack = LFloat (fromIntegral a) : stack'}
          "round" -> do
            (LFloat a, stack') <- consume1 LFloatT stack
            pure $ state {lStack = LInteger (round a) : stack'}
          "!" -> do
            (LLabel a, b, stack') <- consume2 LLabelT AnyT stack
            let newDict = M.insert a b dict
            pure $ state {lDict = newDict, lStack = stack'}
          "@" -> do
            (LLabel a, stack') <- consume1 LLabelT stack
            let lookupWord = dict ! a
            pure $ state {lStack = lookupWord : stack'}
          "forget" -> do
            (LLabel a, stack') <- consume1 LLabelT stack
            let newDict = M.delete a dict
            pure $ state {lStack = stack', lDict = newDict}
          "." -> do
            (a, stack') <- consume1 AnyT stack
            liftIO . putStrLn $ reprWord a
            pure $ state {lStack = stack'}
          "s." -> do
            (wordP, stack') <- consume1 LPhraseT stack
            let LPhrase ws = wordP
            let isLChar w = case w of LChar _ -> True; _ -> False
            unless (all isLChar ws) $ throwError $ LException $ fmt "cannot string-print heterogenous or non-string phrase: %%" [reprWord wordP]
            let stringRepr = ws $> map (\(LChar c) -> c)
            liftIO . putStrLn $ stringRepr
            pure $ state {lStack = stack'}
          "?" -> do
            (LLabel a, stack') <- consume1 LLabelT stack
            let lookupWord = dict ! a
            liftIO . putStrLn $ reprWord lookupWord
            pure $ state {lStack = stack'}
          "+" -> do
            (a, b, stack') <- consume2 AnyT AnyT stack
            result <- lAddNumbers b a
            pure $ state {lStack = result : stack'}
          "-" -> do
            (a, b, stack') <- consume2 AnyT AnyT stack
            result <- lSubNumbers b a
            pure $ state {lStack = result : stack'}
          "*" -> do
            (a, b, stack') <- consume2 AnyT AnyT stack
            result <- lMultiplyNumbers b a
            pure $ state {lStack = result : stack'}
          "/" -> do
            (a, b, stack') <- consume2 AnyT AnyT stack
            result <- lDivideNumbers b a
            pure $ state {lStack = result : stack'}
          "mod" -> do
            (a, b, stack') <- consume2 AnyT AnyT stack
            result <- lModNumbers b a
            pure $ state {lStack = result : stack'}
          "eq?" -> do
            (a, b, stack') <- consume2 AnyT AnyT stack
            pure $ state {lStack = LBool (a == b) : stack'}
          "gt?" -> do
            (a, b, stack') <- consume2 AnyT AnyT stack
            result <- lGreaterThan b a
            pure $ state {lStack = result : stack'}
          "lt?" -> do
            (a, b, stack') <- consume2 AnyT AnyT stack
            result <- lLesserThan b a
            pure $ state {lStack = result : stack'}
          "unphrase" -> do
            (LPhrase phrase, stack') <- consume1 LPhraseT stack
            pure $ state {lSource = phrase ++ source, lStack = stack'}
          "phrase" -> do
            (LSymbol "]", stack') <- consume1 LSymbolT stack
            (body, _ : newStack) <- safeBreak (== LSymbol "[") (LException "phrase-start marker '[' missing in stack") stack'
            pure $ state {lStack = LPhrase (reverse body) : newStack}
          "repr" -> do
            (word, stack') <- consume1 AnyT stack
            let reprStr = reprWord word $> map LChar .> LPhrase
            pure $ state {lStack = reprStr : stack'}
          "pop" -> do
            (LPhrase phrase, stack') <- consume1 LPhraseT stack
            let (first : rest) = phrase
            pure $ state {lStack = first : LPhrase rest : stack'}
          "stack-size" ->
            let size = LInteger $ fromIntegral (length stack)
             in pure $ state {lStack = size : stack}
          "']" ->
            pure $ state {lStack = LSymbol "]" : stack}
          "'[" ->
            pure $ state {lStack = LSymbol "[" : stack}
          "cond" -> do
            (fb, tb, LBool cond, stack') <- consume3 LPhraseT LPhraseT LBoolT stack
            let LPhrase branch = if cond then tb else fb
            let newSource = branch ++ source
            pure $ state {lStack = stack', lSource = newSource}
          "loop" -> do
            (bodyP, condP, stack') <- consume2 LPhraseT LPhraseT stack
            let (LPhrase body, LPhrase cond) = (bodyP, condP)
            let ifWords =
                  concat
                    [ cond,
                      [LPhrase (body ++ [condP, bodyP, LSymbol "loop"])],
                      [LPhrase [], LSymbol "cond"]
                    ]
            let newSource = ifWords ++ source
            pure $ state {lStack = stack', lSource = newSource}
          _ -> throwError $ LException $ fmt "not defined: %%" [symbol]
  | otherwise = pure $ state {lStack = word : stack}

lAddNumbers :: LWord -> LWord -> ExceptT LException IO LWord
lAddNumbers (LInteger a) (LInteger b) = pure $ LInteger (a + b)
lAddNumbers (LFloat a) (LFloat b) = pure $ LFloat (a + b)
lAddNumbers a b = throwError $ LException $ fmt "sum is not defined for %%, %%" [show a, show b]

lSubNumbers :: LWord -> LWord -> ExceptT LException IO LWord
lSubNumbers (LInteger a) (LInteger b) = pure $ LInteger (a - b)
lSubNumbers (LFloat a) (LFloat b) = pure $ LFloat (a - b)
lSubNumbers a b = throwError $ LException $ fmt "difference is not defined for %%, %%" [show a, show b]

lMultiplyNumbers :: LWord -> LWord -> ExceptT LException IO LWord
lMultiplyNumbers (LInteger a) (LInteger b) = pure $ LInteger (a * b)
lMultiplyNumbers (LFloat a) (LFloat b) = pure $ LFloat (a * b)
lMultiplyNumbers a b = throwError $ LException $ fmt "product is not defined for %%, %%" [show a, show b]

lDivideNumbers :: LWord -> LWord -> ExceptT LException IO LWord
lDivideNumbers (LInteger a) (LInteger b) =
  case b of
    0 -> throwError $ LException "division by zero"
    _ -> pure $ LInteger (a `div` b)
lDivideNumbers (LFloat a) (LFloat b) =
  case b of
    0 -> throwError $ LException "division by zero"
    _ -> pure $ LFloat (a / b)
lDivideNumbers a b = throwError $ LException $ fmt "division is not defined for %%, %%" [show a, show b]

lGreaterThan :: LWord -> LWord -> ExceptT LException IO LWord
lGreaterThan (LInteger a) (LInteger b) = pure $ LBool (a > b)
lGreaterThan (LFloat a) (LFloat b) = pure $ LBool (a > b)
lGreaterThan a b = throwError $ LException $ fmt "greater-than is not defined for %%, %%" [show a, show b]

lLesserThan :: LWord -> LWord -> ExceptT LException IO LWord
lLesserThan (LInteger a) (LInteger b) = pure $ LBool (a < b)
lLesserThan (LFloat a) (LFloat b) = pure $ LBool (a < b)
lLesserThan a b = throwError $ LException $ fmt "lesser-than is not defined for %%, %%" [show a, show b]

lModNumbers :: LWord -> LWord -> ExceptT LException IO LWord
lModNumbers (LInteger a) (LInteger b) = pure $ LInteger (a `mod` b)
lModNumbers a b = throwError $ LException $ fmt "modulo is not defined for %%, %%" [show a, show b]
