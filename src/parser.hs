module Parser where

import qualified Data.Bifunctor as B
import qualified Data.Hashable as DH
import Data.List (intercalate, isInfixOf, isPrefixOf)
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

processStringLiterals = processStringLiterals' Nothing M.empty

processStringLiterals' :: Maybe String -> Map String [LWord] -> String -> (String, Map String [LWord])
processStringLiterals' currentM refMap [] = case currentM of
  Just _ -> error "[error] nonterminated string literal"
  Nothing -> ([], refMap)
processStringLiterals' currentM refMap (c : source) = case c of
  '"' -> case currentM of
    Just str ->
      -- string literal ends
      let hash = DH.hash str $> show
          strP = str $> map LChar .> LPhrase
          interpolated = processSLInterpolations strP
          newRefMap = M.insert hash interpolated refMap
          (resSource, resRefMap) = processStringLiterals' Nothing newRefMap source
       in ("##" ++ hash ++ resSource, resRefMap)
    Nothing ->
      -- string literal starts
      processStringLiterals' (Just "") refMap source
  other -> case currentM of
    Just str -> processStringLiterals' (Just $ str ++ [c]) refMap source -- add char to current string literal
    Nothing -> processStringLiterals' Nothing refMap source $> B.first (c :) -- proceed normally

processSLInterpolations :: LWord -> [LWord]
processSLInterpolations strP@(LPhrase lChars)
  | "%%" `isInfixOf` chars = [LPhrase interpolated, LSymbol "unphrase"]
  | otherwise = [strP]
  where
    chars = lChars $> map (\(LChar c) -> c)
    tChars = T.pack chars
    separatedByMarker = T.pack chars $> T.splitOn (T.pack "%%") .> map (T.unpack .> map LChar)
    labelIndices = [0 .. length separatedByMarker - 1 - 1]
    varStores = labelIndices $> map (\n -> [LLabel ("$__" ++ show n), LSymbol "!"]) .> reverse
    part1 = concat varStores
    symbolLookups = labelIndices $> map (\n -> [LLabel ("$__" ++ show n), LSymbol "@", LSymbol "unphrase"])
    part2 = LSymbol "'[" : concat (mix separatedByMarker symbolLookups) ++ [LSymbol "']", LSymbol "phrase"]
    varForgets = labelIndices $> map (\n -> [LLabel ("$__" ++ show n), LSymbol "forget"])
    part3 = concat varForgets
    interpolated = part1 ++ part2 ++ part3
processSLInterpolations p = error $ "[error] non-phrase in processSLInterpolations: " ++ show p

parseSource source =
  let woComments = source $> removeComments
      (woStringLiterals, stringLiteralRefMap) = processStringLiterals woComments
   in (woStringLiterals $> words .> map parseWord, stringLiteralRefMap)
