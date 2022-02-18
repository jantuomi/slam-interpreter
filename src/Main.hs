module Main where

import Control.Monad.Except
import Data.Char
import Data.List (intercalate)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Debug.Trace (trace, traceShow)
import Interpreter
import LTypes
import Parser
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin)
import Utils

-- | Parse command line options and positional arguments, and
-- apply them to the supplied config
getConfig config [] = config
getConfig config ("--debug" : rest) =
  let newConfig = config {configDebugMode = True}
   in getConfig newConfig rest
getConfig config (fileName : rest) =
  let newConfig = config {configFileNameM = Just fileName}
   in getConfig newConfig rest

-- | Extract source file name from config. If not defined,
-- throw Exception
getFileName :: Config -> ExceptT LException IO String
getFileName Config {configFileNameM = fileNameM} = do
  case fileNameM of
    Just str -> pure str
    Nothing -> throwError $ LException "no filename specified"

-- | Setup config based on args,
-- parse the source,
-- construct initial interpreter state,
-- and start interpreting by calling interpretSource
bootstrap :: [String] -> ExceptT LException IO ()
bootstrap args = do
  let initialConfig =
        Config
          { configFileNameM = Nothing,
            configDebugMode = False
          }

  let config = getConfig initialConfig args
  fileName <- getFileName config

  -- Print debug information about the parse result if in debug mode
  debugPrint config $ fmt "executing file %%\n" [fileName]
  source <- liftIO . readFile $ fileName
  (parsedSource, strLitRefMap) <- parseSource source
  debugPrint config $ fmt "parsed words:\n%%\n" [parsedSource $> map show .> intercalate ", "]
  let reprKeyValPair (k, v) = fmt "%% = %%" [k, show v]
  debugPrint config $
    fmt
      "string literal refmap:\n%%\n"
      [ strLitRefMap
          $> M.toList .> map reprKeyValPair .> intercalate "\n"
      ]
  debugPrint config "interpreter output:"

  -- Construct initial state
  let initialState =
        LState
          { lDict = M.empty,
            lStack = [],
            lPhraseDepth = 0,
            lDefs = M.empty,
            lSource = parsedSource,
            lStrLitRefMap = strLitRefMap
          }

  -- Start interpreting source
  void $ interpretSource config initialState

-- | Entrypoint: read args and call bootstrap
-- Upon exception, the exception will be printed to stdout and the
-- program will be terminated with a non-zero status
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  args <- getArgs
  result <- runExceptT $ bootstrap args
  case result of
    Left (LException errStr) -> do
      putStrLn $ "[error] " ++ errStr
      exitFailure
    _ -> exitSuccess
