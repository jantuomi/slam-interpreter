module Main where

import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as M
import Debug.Trace (trace, traceShow)
import Interpreter
import LTypes
import Parser
import System.Environment (getArgs)
import Utils

getConfig config [] = config
getConfig config ("--debug" : rest) =
  let newConfig = config {configDebugMode = True}
   in getConfig newConfig rest
getConfig config (fileName : rest) =
  let newConfig = config {configFileNameM = Just fileName}
   in getConfig newConfig rest

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
