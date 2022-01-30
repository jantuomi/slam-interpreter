module Main where

import Control.Monad.Except
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as M
import Debug.Trace (trace, traceShow)
import Interpreter
import LTypes
import Parser
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Utils

getConfig config [] = config
getConfig config ("--debug" : rest) =
  let newConfig = config {configDebugMode = True}
   in getConfig newConfig rest
getConfig config (fileName : rest) =
  let newConfig = config {configFileNameM = Just fileName}
   in getConfig newConfig rest

getFileName :: Config -> ExceptT LException IO String
getFileName Config {configFileNameM = fileNameM} = do
  case fileNameM of
    Just str -> pure str
    Nothing -> throwError $ LException "no file name specified"

bootstrap :: [String] -> ExceptT LException IO ()
bootstrap args = do
  let initialConfig =
        Config
          { configFileNameM = Nothing,
            configDebugMode = False
          }
  let config = getConfig initialConfig args
  fileName <- getFileName config

  debugPrint config $ "executing file " ++ fileName ++ "\n"
  source <- liftIO . readFile $ fileName
  (parsedSource, strLitRefMap) <- parseSource source
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

main :: IO ()
main = do
  args <- getArgs
  result <- runExceptT $ bootstrap args
  case result of
    Left ex -> case ex of
      LException errStr -> do
        putStrLn $ "[error] " ++ errStr
        exitFailure
    _ -> exitSuccess
