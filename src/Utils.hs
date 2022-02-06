module Utils where

import Control.Monad.Except
import qualified Data.Bifunctor as B
import Data.Text (Text)
import qualified Data.Text as T

(.>) = flip (.)

($>) = flip ($)

infixr 6 $>

countCond :: (a -> Bool) -> [a] -> Int
countCond cond list = filter cond list $> length

occursTimes :: Eq a => a -> [a] -> Int
occursTimes needle = countCond (== needle)

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

isFloatStr str =
  let ir = readMaybe str :: Maybe Double
   in case ir of
        Just _ -> True
        Nothing -> False

isIntStr str =
  let ir = readMaybe str :: Maybe Integer
   in case ir of
        Just _ -> True
        Nothing -> False

isCharStr str =
  case str of
    ['\'', c, '\''] -> True
    _ -> False

breakOn :: (a -> Bool) -> [a] -> ([a], [a])
breakOn cond xs = break cond xs $> B.second (drop 1)

mix :: [a] -> [a] -> [a]
mix (x : xs) (y : ys) = x : y : mix xs ys
mix x [] = x
mix [] y = y

safeBreak cond ex = safeBreak' cond ex []

safeBreak' _ ex _ [] = throwError ex
safeBreak' cond ex acc lst@(x : xs)
  | cond x = pure (reverse acc, lst)
  | otherwise = safeBreak' cond ex (x : acc) xs

fmt :: String -> [String] -> String
fmt str values = T.unpack $ fmt' (T.pack str) (map T.pack values)

fmt' :: Text -> [Text] -> Text
fmt' text [] = text
fmt' text (v : rest) =
  let pattern = T.pack "%%"
      (front, back) = T.breakOn pattern text
      res = T.concat [front, v, T.drop (T.length pattern) back]
   in fmt' res rest
