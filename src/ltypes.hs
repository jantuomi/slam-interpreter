module LTypes where

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
