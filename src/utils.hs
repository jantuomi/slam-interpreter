module Utils where
import qualified Data.Bifunctor as B

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

breakOn :: (a -> Bool) -> [a] -> ([a], [a])
breakOn cond xs = break cond xs $> B.second (drop 1)
