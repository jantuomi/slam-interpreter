module Parser where

import qualified Data.Bifunctor as B
import qualified Data.Hashable as DH
import Data.List (isPrefixOf)
import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.Text as T
import LTypes
import Utils

parseWord rawStr
  | isIntStr rawStr = LInteger (read rawStr)
  | isFloatStr rawStr = LFloat $ read rawStr
  | isCharStr rawStr = LChar $ rawStr !! 1
  | "##" `isPrefixOf` rawStr = LStringLitRef $ drop 2 rawStr
  | "$" `isPrefixOf` rawStr = LLabel $ tail rawStr
  | otherwise = LSymbol rawStr

removeComments source =
  let lines_ = lines source $> map (T.pack .> T.splitOn (T.pack "--") .> head .> T.unpack)
   in unlines lines_

processStringLiterals source =
  let refMap = M.empty
   in processStringLiterals' Nothing refMap source

processStringLiterals' :: Maybe String -> Map String String -> String -> (String, Map String String)
processStringLiterals' currentM refMap [] = case currentM of
  Just _ -> error "[error] nonterminated string literal"
  Nothing -> ([], refMap)
processStringLiterals' currentM refMap (c : source) = case c of
  '"' -> case currentM of
    Just str ->
      let hash = DH.hash str $> show
          newRefMap = M.insert hash str refMap
          (resSource, resRefMap) = processStringLiterals' Nothing newRefMap source
       in ("##" ++ hash ++ resSource, resRefMap)
    Nothing ->
      processStringLiterals' (Just "") refMap source
  other -> case currentM of
    Just str -> processStringLiterals' (Just $ str ++ [c]) refMap source
    Nothing -> processStringLiterals' Nothing refMap source $> B.first (c :)

parseSource source =
  let woComments = source $> removeComments
      (woStringLiterals, stringLiteralRefMap) = processStringLiterals woComments
   in (woStringLiterals $> words .> map parseWord, stringLiteralRefMap)
