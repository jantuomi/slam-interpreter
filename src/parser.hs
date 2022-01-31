module Parser where

import Control.Monad.Except
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

processStringLiterals :: Maybe String -> Map String [LWord] -> String -> ExceptT LException IO (String, Map String [LWord])
processStringLiterals currentM refMap [] = case currentM of
  Just _ -> throwError $ LException "nonterminated string literal"
  Nothing -> pure ([], refMap)
processStringLiterals currentM refMap (c : source) = case c of
  '"' -> case currentM of
    Just str -> do
      -- string literal ends
      let hash = DH.hash str $> show
      let strP = str $> map LChar .> LPhrase
      interpolated <- processSLInterpolations strP
      let newRefMap = M.insert hash interpolated refMap
      (resSource, resRefMap) <- processStringLiterals Nothing newRefMap source
      pure ("##" ++ hash ++ resSource, resRefMap)
    Nothing ->
      -- string literal starts
      processStringLiterals (Just "") refMap source
  other -> case currentM of
    Just str ->
      -- add char to current string literal
      processStringLiterals (Just $ str ++ [c]) refMap source
    Nothing ->
      -- proceed normally
      processStringLiterals Nothing refMap source $> fmap (B.first (c :))

processSLInterpolations :: LWord -> ExceptT LException IO [LWord]
processSLInterpolations strP@(LPhrase lChars)
  | "%%" `isInfixOf` chars = pure [LPhrase interpolated, LSymbol "unphrase"]
  | otherwise = pure [strP]
  where
    chars = lChars $> map (\(LChar c) -> c)
    -- split string on the %% marker into n substrings
    separatedByMarker = T.pack chars $> T.splitOn (T.pack "%%") .> map (T.unpack .> map LChar)
    labelIndices = [0 .. length separatedByMarker - 1 - 1]
    -- store n - 1 variables from stack
    varStores = labelIndices $> map (\n -> [LLabel ("$__" ++ show n), LSymbol "!"]) .> reverse
    part1 = concat varStores
    -- interleave the n substrings and n - 1 variable reads
    symbolLookups = labelIndices $> map (\n -> [LLabel ("$__" ++ show n), LSymbol "@", LSymbol "unphrase"])
    part2 = LSymbol "'[" : concat (mix separatedByMarker symbolLookups) ++ [LSymbol "']", LSymbol "phrase"]
    -- forget the temp variables used
    varForgets = labelIndices $> map (\n -> [LLabel ("$__" ++ show n), LSymbol "forget"])
    part3 = concat varForgets
    -- combine everything into one phrase
    interpolated = part1 ++ part2 ++ part3
processSLInterpolations p = throwError $ LException $ "non-phrase in processSLInterpolations: " ++ show p

parseSource source = do
  let woComments = source $> removeComments
  (woStringLiterals, stringLiteralRefMap) <- processStringLiterals Nothing M.empty woComments
  pure (woStringLiterals $> words .> map parseWord, stringLiteralRefMap)