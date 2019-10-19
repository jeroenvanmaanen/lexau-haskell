-- |Named objects.
module LExAu.API.Named (Named(name)) where

-- |Methods for named objects.
class Named named where
  name :: named -> String

-- EOF
