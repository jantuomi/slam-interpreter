module LTypes where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import Utils

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

data LWordT = LSymbolT | LIntegerT | LFloatT | LBoolT | LCharT | LLabelT | LPhraseT | AnyT deriving (Show)

consumeErr wordT word = LException $ "expected " ++ show wordT ++ ", encountered " ++ show word

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

consume2 :: LWordT -> LWordT -> [LWord] -> ExceptT LException IO (LWord, LWord, [LWord])
consume2 wordT1 wordT2 stack = do
  (ret1, stack1) <- consume1 wordT1 stack
  (ret2, stack2) <- consume1 wordT2 stack1
  pure (ret1, ret2, stack2)

consume3 :: LWordT -> LWordT -> LWordT -> [LWord] -> ExceptT LException IO (LWord, LWord, LWord, [LWord])
consume3 wordT1 wordT2 wordT3 stack = do
  (ret1, stack1) <- consume1 wordT1 stack
  (ret2, stack2) <- consume1 wordT2 stack1
  (ret3, stack3) <- consume1 wordT3 stack2
  pure (ret1, ret2, ret3, stack3)

reprWord :: LWord -> String
reprWord (LSymbol a) = a
reprWord (LInteger a) = show a
reprWord (LFloat a) = show a
reprWord (LBool a) = show a
reprWord (LChar a) = show a
reprWord (LLabel a) = a
reprWord (LStringLitRef a) = "StrLit(" ++ a ++ ")"
reprWord (LPhrase a) = "P[ " ++ map reprWord a $> unwords ++ " ]"

data LState = LState
  { lDict :: Map String LWord,
    lStack :: [LWord],
    lPhraseDepth :: Int,
    lDefs :: Map String [LWord],
    lSource :: [LWord],
    lStrLitRefMap :: Map String [LWord]
  }
  deriving (Show)

data Config = Config
  { configFileNameM :: Maybe String,
    configDebugMode :: Bool
  }

data ExecStep = ExecContinue | ExecExit

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
