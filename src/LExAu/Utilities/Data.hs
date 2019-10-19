
module LExAu.Utilities.Data
  ( flodder
  , flodderWithKey
  , select
  , tryToFind
  ) where

import Data.IntMap (IntMap, Key)
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Foldable as Fold (toList)
import qualified Data.IntMap as IntMap (assocs, lookup)

select :: (Num markerType, Ord markerType) => [(key, markerType)] -> markerType -> key
select ((key, value) : pairs) dial =
  if dial <= value
  then key
  else select pairs (dial - value)

flodder :: (a -> b -> b) -> b -> [a] -> b
flodder f z xs = foldl' (flip f) z xs

flodderWithKey :: (Key -> a -> b -> b) -> b -> IntMap a -> b
flodderWithKey f z m = flodder g z (IntMap.assocs m) where g (k, v) a = f k v a

tryToFind :: (Show memberType) => Seq String -> Int -> IntMap memberType -> memberType
tryToFind labels idx mapping =
  case idx `IntMap.lookup` mapping of
    Just member -> member
    Nothing ->
      let prefix = labels |> "Not found"
          showList = map showString $ concatMap (:[": "]) $ Fold.toList prefix
          message = ((foldr (.) id showList) . shows mapping . showChar '[' . shows idx) "]"
      in error message

-- EOF
