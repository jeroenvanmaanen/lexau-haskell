{
{-# LANGUAGE BangPatterns #-}
module Data.PrettyPrint.Parens
  ( Token(..)
  , TokenType(..)
  , alexScanTokens2
  ) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$open = [\[\{\(]
$close = [\]\}\)]
$separator = [\,\;]

tokens :-

  $white+                       { \ p s -> T Space p " " }
  \" ( [^\"] | \\ \" )* \"      { T Quoted }	
  \' ( [^\'] | \\ \' )* \'      { T Quoted }	
  $open                         { T Open }
  $close                        { T Close }
  $separator                    { T Sep }
  [^ $white $open $close $separator \"\']+  { T Word }

{

-- The token type
data TokenType
  = Open
  | Close
  | Sep
  | Word
  | Quoted
  | Space
  deriving (Eq, Show)

data Token = T { theType :: TokenType, thePosition :: AlexPosn, theValue :: String } deriving (Eq, Show)

-- An alternative to alexscantokens that reports error context
getOffset :: AlexPosn -> Int
getOffset (AlexPn offset lineNum colNum) = offset

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn offset lineNum colNum) = lineNum

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn offset lineNum colNum) = colNum

mySubString :: Int -> Int -> String -> String
mySubString from to s =
  if from > 0
    then mySubString 0 (to - from) $ drop from s
    else take to s

alexScanTokens2 :: String -> [Token]
alexScanTokens2 str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ ->
                  let offset = getOffset pos
                  in error ("lexical error @ line " ++ show (getLineNum(pos)) ++ " and column " ++ show (getColumnNum(pos)) ++ " context '" ++ (mySubString 0 10 str) ++ "'")
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'
 
}
