module LTypes where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import Utils

-- | An LWord is a typed value that can read from the source and
-- pushed onto the stack. LWords are the core data structure.
data LWord
  = LSymbol String
  | LInteger Integer
  | LFloat Double
  | LBool Bool
  | LChar Char
  | LLabel String
  | LStringLitRef String
  | LPhrase [LWord]
  deriving (Show, Eq)

-- | An LWordT represents the type of an 'LWord'. Each LWord has one corresponding LWordT,
-- expect for the LWordT AnyT, which represents any LWord value.
data LWordT = LSymbolT | LIntegerT | LFloatT | LBoolT | LCharT | LLabelT | LPhraseT | AnyT deriving (Show)

-- | Produce an exception referencing an expected 'LWordT' and the encountered 'LWord'
consumeErr wordT word = LException $ "expected " ++ show wordT ++ ", encountered " ++ show word

-- | Attempt to consume one 'LWord' from the supplied stack. If the word at the top of the stack matches
-- the supplied 'LWordT', return that word and the updated stack. If not, return an exception.
consume1 :: LWordT -> [LWord] -> ExceptT LException IO (LWord, [LWord])
consume1 wordT [] =
  throwError $ LException $ "expected " ++ show wordT ++ ", encountered empty stack"
consume1 AnyT (word : stack') = pure (word, stack')
consume1 wordT@LSymbolT (word : stack') =
  case word of LSymbol _ -> pure (word, stack'); _ -> throwError $ consumeErr wordT word
consume1 wordT@LIntegerT (word : stack') =
  case word of LInteger _ -> pure (word, stack'); _ -> throwError $ consumeErr wordT word
consume1 wordT@LFloatT (word : stack') =
  case word of LFloat _ -> pure (word, stack'); _ -> throwError $ consumeErr wordT word
consume1 wordT@LBoolT (word : stack') =
  case word of LBool _ -> pure (word, stack'); _ -> throwError $ consumeErr wordT word
consume1 wordT@LCharT (word : stack') =
  case word of LChar _ -> pure (word, stack'); _ -> throwError $ consumeErr wordT word
consume1 wordT@LLabelT (word : stack') =
  case word of LLabel _ -> pure (word, stack'); _ -> throwError $ consumeErr wordT word
consume1 wordT@LPhraseT (word : stack') =
  case word of LPhrase _ -> pure (word, stack'); _ -> throwError $ consumeErr wordT word

-- | Invokes 'consume1' twice.
consume2 :: LWordT -> LWordT -> [LWord] -> ExceptT LException IO (LWord, LWord, [LWord])
consume2 wordT1 wordT2 stack = do
  (ret1, stack1) <- consume1 wordT1 stack
  (ret2, stack2) <- consume1 wordT2 stack1
  pure (ret1, ret2, stack2)

-- | Invokes 'consume1' three times.
consume3 :: LWordT -> LWordT -> LWordT -> [LWord] -> ExceptT LException IO (LWord, LWord, LWord, [LWord])
consume3 wordT1 wordT2 wordT3 stack = do
  (ret1, stack1) <- consume1 wordT1 stack
  (ret2, stack2) <- consume1 wordT2 stack1
  (ret3, stack3) <- consume1 wordT3 stack2
  pure (ret1, ret2, ret3, stack3)

-- | Convert 'LWord' contents to a user-friendly string representation.
reprWord :: LWord -> String
reprWord (LSymbol a) = a
reprWord (LInteger a) = show a
reprWord (LFloat a) = show a
reprWord (LBool a) = show a
reprWord (LChar a) = show a
reprWord (LLabel a) = a
reprWord (LStringLitRef a) = "StrLit(" ++ a ++ ")"
reprWord (LPhrase a) = "P[ " ++ map reprWord a $> unwords ++ " ]"

-- | Represents the interpreter state. The state changes one processed 'LWord' at a time.
-- Contains:
-- * 'lDict': A mapping from 'LLabel' string values to 'LWord'. Used for storing variables.
-- * 'lStack': A list of 'LWord's, representing the global stack. The head is the top of the stack.
-- * 'lPhraseDepth': A number representing how many layers deep the current phrase context is. If zero (no phrase),
--    words are immediately evaluated when encountered.
-- * 'lDefs': A mapping from 'LSymbol' string values to phrases of 'LWord's. Used for defining custom words.
-- * 'lSource': A list of words yet to be processed. The head will be processed first.
-- * 'lStrLitRefMap': A map from string (hash digest) to list of words. String literals in the source code
--   are replaced by a hash that is replaced with the refmap content upon evaluation. The refmap is populated
--   during parsing (literal desugaring).
data LState = LState
  { lDict :: Map String LWord,
    lStack :: [LWord],
    lPhraseDepth :: Int,
    lDefs :: Map String [LWord],
    lSource :: [LWord],
    lStrLitRefMap :: Map String [LWord]
  }
  deriving (Show)

-- | Represents command line options and arguments, provided by the user.
data Config = Config
  { configFileNameM :: Maybe String,
    configDebugMode :: Bool
  }

data ExecStep = ExecContinue | ExecExit

-- | If in debug mode, dump state to stdout. Otherwise, no op.
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

newtype LException = LException String
