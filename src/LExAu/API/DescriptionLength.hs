-- |Interface for objects that have a well-defined description length.
module LExAu.API.DescriptionLength (
    DescriptionLength(descriptionLength),
    PrefixEncode(prefixEncode),
    toTerseCode
  ) where

import Data.Maybe (maybeToList)

-- |Object that has a well-defined description length.
class DescriptionLength objectType where

  -- |Returns the description length of the given object.
  descriptionLength :: objectType -> Integer

-- |Object that can be described with a prefix code
class PrefixEncode objectType where

  -- |Returns the prefix code for the given object
  prefixEncode :: objectType -> ShowS

toTerseCode :: String -> String
toTerseCode verboseCode =
  concatMap (maybeToList . flip lookup [('O', '0'), ('I', '1')]) verboseCode
