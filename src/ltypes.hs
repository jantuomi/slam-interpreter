module LTypes where

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
