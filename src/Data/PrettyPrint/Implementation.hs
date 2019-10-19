
module Data.PrettyPrint.Implementation
  ( prettyPrint
  , test
  ) where

import Text.Show
  ( showListWith
  )

import Data.Foldable (toList)
import Data.Functor (fmap)
import Data.Sequence (Seq, viewl, viewr, (<|), (|>), ViewL(EmptyL, (:<)), ViewR(EmptyR, (:>)))
import Debug.Trace (traceShow)
import System.IO (readFile)
import qualified Data.Foldable as Fold (foldr)
import qualified Data.Sequence as Seq (empty, null, singleton)

import Data.PrettyPrint.Parens
  ( Token(T, theType,theValue)
  , TokenType(..)
  , alexScanTokens2
  )

data PPToken
  = PToken String
  | PBreak String String String -- unbroken, pre-break, post-break
  | POpen String
  | PClose
  | PCollapsed String String
  | PEOT
  deriving (Eq, Show)

data Chunk
  = Chunk
      { theTokens :: Seq PPToken
      , theLength :: Int
      , theContinuationFlag :: Bool
      }
  deriving (Eq, Show)

emptyChunk = Chunk Seq.empty 0 False

data Sugar
  = SugarCube String
  | SugarPot ShowS
instance Show (Sugar) where
  showsPrec _ (SugarCube s) = showString s
  showsPrec _ (SugarPot s) = s

newLineSugar = SugarCube "\n"
(+|+) a b = SugarPot $ shows a . shows b

traceShowX :: (Show b) => b -> a -> a
--traceShowX = traceShow
traceShowX m x = x

addChunkToken :: Chunk -> PPToken -> Chunk

addChunkToken oldChunk@(Chunk oldTokens oldLength _) token =
  let newTokens = oldTokens |> token
      newLength = oldLength + (tokenLength token)
  in oldChunk { theTokens = newTokens, theLength = newLength }

addChunk :: Seq Chunk -> Seq Chunk

addChunk bufferStack =
  bufferStack |> emptyChunk

stackLength :: [String] -> Seq Chunk -> Int

stackLength indentation bufferStack =
  let indentLength = length $ concat indentation
      chunksLength = foldr (+) 0 $ map theLength $ toList bufferStack
  in indentLength + chunksLength

tokenLength :: PPToken -> Int

tokenLength (PToken s) = length s

tokenLength (PBreak b _ _) = length b

tokenLength (PCollapsed _ c) = length c

tokenLength _ = 0

toPToken :: Token -> PPToken

toPToken token =
  case theType token of
    Space -> PBreak " " "" "" -- (theValue token) "" "" --
    _ -> PToken (theValue token)

emptyBreak :: PPToken
emptyBreak = PBreak "" "" ""

addToken :: PPToken -> [PPToken] -> [PPToken]

addToken first@(PToken value1) pptokens@(PToken value2 : remainder) =
  (PToken (value1 ++ value2)) : remainder

addToken first@(PBreak unbroken1 preBreak1 postBreak1) pptokens@(PBreak unbroken2 preBreak2 postBreak2 : remainder) =
  if (length postBreak1 > 0) && (length preBreak2 > 0)
    then first : pptokens
    else (PBreak (unbroken1 ++ unbroken2) (preBreak1 ++ preBreak2) (postBreak1 ++ postBreak2)) : remainder

addToken open@(POpen _) (PClose : remainder) = remainder

addToken open@(POpen _) (brk@(PBreak _ _ _) : remainder) = addTokens [brk, open] remainder

addToken PClose (brk@(PBreak _ _ _) : remainder) = addTokens [brk, PClose] remainder

addToken first pptokens = first : pptokens

data SepState
  = SepClean
  | SepReady Bool -- eat another token or not
  | SepOpen

addTokens :: [PPToken] -> [PPToken] -> [PPToken]

addTokens newTokens oldTokens =
  foldr addToken oldTokens newTokens

toPPTokens :: String -> [Token] -> [SepState] -> [PPToken] -> [PPToken]

toPPTokens indent tokens sepStateStack [] = toPPTokens indent tokens sepStateStack [PEOT]

toPPTokens indent [] (SepClean : sss) pptokens@(_:_) = toPPTokens indent [] sss pptokens
toPPTokens indent [] (SepReady _ : sss) pptokens@(_:_) = toPPTokens indent [] sss (addTokens [PClose] pptokens)
toPPTokens indent [] (SepOpen : sss) pptokens@(_:_) = toPPTokens indent [] sss (addTokens [PClose, PClose] pptokens)
toPPTokens _ [] [] pptokens@(_:_) = pptokens

toPPTokens indent (token : tokens) sepStateStack pptokens@(_:_) =
  let (newSepStateStack, sepPTokens) = updateSepStack (markIndent '.' indent) sepStateStack $ theType token
      remainder = toPPTokens indent tokens newSepStateStack pptokens
      (pTokensToAddBeforeSep, pTokensToInsertInSep, pTokensToAddAfterSep) =
        case theType token of
         Open -> ([], [toPToken token], [POpen indent])
         Close -> ([PClose], [], [toPToken token])
         Sep -> ([], [], [toPToken token, emptyBreak])
         _ -> ([], [], [toPToken token])
      (firstSepPTokens, otherSepPTokens) =
        case sepPTokens of
          t : [] -> ([] ,[t])
          t : ts -> ([t], ts)
          _ -> ([], [])
      allPTokensToAdd = pTokensToAddBeforeSep ++ firstSepPTokens ++ pTokensToInsertInSep ++ otherSepPTokens ++ pTokensToAddAfterSep
  in addTokens allPTokensToAdd remainder

updateSepStack :: String -> [SepState] -> TokenType -> ([SepState], [PPToken])

updateSepStack _ (SepReady _ : sss) Open = (SepClean : (SepClean : sss), [PClose])
updateSepStack _ (SepOpen : sss) Open = (SepClean : (SepClean : sss), [PClose, PClose])
updateSepStack _ sss Open = (SepClean : sss, [])
updateSepStack indent (SepOpen : sss) Close = updateSepStackClose indent sss [PClose, PClose]
updateSepStack indent (SepReady _ : sss) Close = updateSepStackClose indent sss [PClose]
updateSepStack indent (SepClean : sss) Close = updateSepStackClose indent sss []
updateSepStack _ (SepOpen : sss) Sep = (SepClean : sss, [PClose, PClose])
updateSepStack _ (_ : sss) Sep = (SepClean : sss, [PClose])
updateSepStack _ (SepClean : sss) Word = (SepReady False : sss, [POpen ""])
updateSepStack _ (SepClean : sss) Quoted = (SepReady False : sss, [POpen ""])
updateSepStack indent (SepReady True : sss) Word = (SepReady False : sss, [])
updateSepStack indent (SepReady True : sss) Quoted = (SepReady False : sss, [])
updateSepStack indent (SepReady False : sss) Word = (SepOpen : sss, [POpen indent])
updateSepStack indent (SepReady False : sss) Quoted = (SepOpen : sss, [POpen indent])
updateSepStack _ sss _ = (sss, [])

updateSepStackClose :: String -> [SepState] -> [PPToken] -> ([SepState], [PPToken])

updateSepStackClose _ (SepClean : sss) tokens = (SepReady True : sss, tokens ++ [POpen ""])
updateSepStackClose indent (SepReady _ : sss) tokens = (SepOpen : sss, tokens ++ [POpen indent])
updateSepStackClose indent sss tokens = (sss, tokens)

prettyPrint :: Int -> String -> String -> ShowS

prettyPrint maxWidth indent line =
  prettyPrintTokens maxWidth indent [] 0 (Seq.singleton emptyChunk) $ toPPTokens indent (alexScanTokens2 line) [SepClean] []

prettyPrintTokens :: Int -> String -> [String] -> Int -> Seq Chunk -> [PPToken] -> ShowS

prettyPrintTokens maxWidth indent indentation width bufferStack [] =
  ppShowPiece maxWidth indent indentation width bufferStack []

prettyPrintTokens maxWidth indent indentation width bufferStack (open@(POpen _) : ts) =
  let newBufferStack = bufferStack |> addChunkToken emptyChunk open
  in prettyPrintTokens maxWidth indent indentation width newBufferStack ts

prettyPrintTokens maxWidth indent indentation width bufferStack (PClose : ts) =
  let parentSeq :> closing = viewr bufferStack
      closed = addChunkToken closing PClose
      (collapsed, maybeBrk) = collapse closed
  in case viewr parentSeq of
          EmptyR -> ppShowPiece maxWidth indent indentation width (Seq.singleton $ closed) ts -- TODO: repeat ppShowPiece and reduce indent
          base :> continued ->
            let nextTop = addChunkToken continued collapsed
                newTop =
                  case maybeBrk of
                    Just brk -> addChunkToken nextTop brk
                    _ -> nextTop
            in traceShowX
                 (newLineSugar +|+ SugarCube "Continued: " +|+ (base |> newTop))
                 prettyPrintTokens maxWidth indent indentation width (base |> newTop) ts

prettyPrintTokens maxWidth indent indentation width bufferStack remainder@(t : ts) =
  let base :> oldOpen = viewr bufferStack
      newOpen = addChunkToken oldOpen t
      extBufferStack = base |> newOpen
      extLength = stackLength indentation extBufferStack
      forceBreak =
        case t of
          (PBreak _ _ _) -> theContinuationFlag newOpen
          _ -> False
  in traceShowX (newLineSugar +|+ (newOpen, extLength, forceBreak)) $
       if extLength > maxWidth || forceBreak
         then ppShowPiece maxWidth indent indentation width extBufferStack ts
         else prettyPrintTokens maxWidth indent indentation width extBufferStack ts

ppShowPiece :: Int -> String -> [String] -> Int -> Seq Chunk -> [PPToken] -> ShowS

ppShowPiece maxWidth indent indentation width bufferStack ts =
  let (firstChunk, remainder) = getChunk bufferStack
      (repr, full, thisIndentation, newIndentation, newChunk) = ppSerializeDelta indent (indentation, firstChunk)
      nextBufferStack =
        if Seq.null $ theTokens newChunk
          then remainder
          else newChunk <| remainder
      newBufferStack =
        if Seq.null nextBufferStack && not (null ts) 
          then Seq.singleton emptyChunk
          else nextBufferStack
      showLine =
        if full
          then showIndentation thisIndentation . repr . showString "\n"
          else id
      tail =
        if Seq.null newBufferStack
          then id
          else prettyPrintTokens maxWidth indent newIndentation width newBufferStack ts
  in showLine .
     tail

-- ppSerializeDelta (oldIndentation, oldChunk) = (representation, full, newIndentation, extra, newChunk)
ppSerializeDelta :: String -> ([String], Chunk) -> (ShowS, Bool, [String], [String], Chunk)

ppSerializeDelta indent (oldIndentation, oldChunk) =
  let oldTokens = theTokens oldChunk
      (repr, full, thisIndentation, newIndentation, newTokens) = ppSerializeDeltaTokens indent (oldIndentation, oldTokens)
      newLength = Fold.foldr (+) 0 $ fmap tokenLength newTokens
  in (repr, full, thisIndentation, newIndentation, oldChunk { theTokens = newTokens, theLength = newLength, theContinuationFlag = True })

-- ppSerializeDeltaTokens (oldIndentation, oldTokens) = (representation, full, newIndentation, extra, newTokens)
ppSerializeDeltaTokens :: String -> ([String], Seq PPToken) -> (ShowS, Bool, [String], [String], Seq PPToken)

ppSerializeDeltaTokens indent (oldIndentation, oldTokens) =
  case viewl oldTokens of
    EmptyL -> (id, False, oldIndentation, oldIndentation, oldTokens)
    (POpen openIndent) :< ts -> ppSerializeDeltaTokens indent (openIndent : oldIndentation, ts)
    PClose :< ts -> (id, False, oldIndentation, tail oldIndentation, ts) -- ppSerializeDeltaTokens (safeTail oldIndentation, ts) -- 
    PBreak _ _ _ :< ts -> (id, False, oldIndentation, oldIndentation, ts)
    t :< ts ->
      let (repr, _, tailIndentation, newIndentation, newTokens) = ppSerializeDeltaTokens indent (oldIndentation, ts)
          thisIndentation =
            case t of
              PCollapsed collapsedIndent _ -> collapsedIndent : oldIndentation
              _ -> tailIndentation
      in (ppSerializeToken t . repr, True, thisIndentation, newIndentation, newTokens)

safeTail :: [a] -> [a]

safeTail [] = []
safeTail a = tail a

ppSerializeToken :: PPToken -> ShowS

ppSerializeToken (PToken s) = showString s

ppSerializeToken (PBreak s _ _) = showString s

ppSerializeToken (POpen _) = id -- showString "[<["

ppSerializeToken PClose = id -- showString "]>]"

ppSerializeToken (PCollapsed _ s) =
  -- showString "$<$" . 
  showString s
  -- . showString "$>$"

ppSerializeToken _ = id

collapse :: Chunk -> (PPToken, Maybe PPToken)

collapse ts =
  let (block, maybeBrk) = rtrim $ theTokens ts
      serializedBlock = (Fold.foldr (.) id $ fmap ppSerializeToken block) ""
      indent =
        case viewl block of
          POpen openIndent :< _ -> openIndent
          _ -> ""
  in (PCollapsed indent serializedBlock, maybeBrk)

rtrim :: Seq PPToken -> (Seq PPToken, Maybe PPToken)

rtrim ts =
  case viewr ts of
    prefix :> brk@(PBreak _ _ _) -> (prefix, Just brk)
    prefix :> PClose ->
      let (block, maybeBrk) = rtrim prefix
      in (block |> PClose, maybeBrk)
    _ -> (ts, Nothing)

showIndentation :: [String] -> ShowS

showIndentation [] = id
showIndentation (s : strings) = showIndentation strings . showString s

markIndent :: Char -> String -> String

--markIndent ch indent = ch : indent
markIndent _ indent = indent

serialize :: [Token] -> ShowS

serialize [] = id
serialize (t : ts) = showString (theValue t) . serialize ts

getChunk :: Seq Chunk -> (Chunk, Seq Chunk)

getChunk bufferStack =
  let firstChunk :< remainder = viewl bufferStack
      trimmed = trimChunk firstChunk
  in if Seq.null (theTokens trimmed) && not (Seq.null remainder)
          then getChunk remainder
          else (trimmed, remainder)

trimChunk :: Chunk -> Chunk

trimChunk oldChunk =
  case viewl (theTokens oldChunk) of
    brk@(PBreak _ _ _) :< ts ->
      let oldLength = theLength oldChunk
          newLength = oldLength - tokenLength brk
      in trimChunk oldChunk { theTokens = ts, theLength = newLength }
    _ -> oldChunk

test :: Maybe String -> IO ()
test maybeFileName =
  do let testString = "try { for (int i = 1; i < 10; i++) { printf(\"i: %d\", i); [] } } catch (Exception with more slow stuff ex) { ExceptionWrapper.wrap(ex); } <?xml lang='en' ?> "
         tokens = alexScanTokens2 testString
         pptokens = toPPTokens "  " tokens [SepClean] []
     putStrLn "Tokens"
     putStrLn $ showListWithNewLine tokens ""
     putStrLn ""
     putStrLn "PPTokens:"
     putStrLn $ showListWithNewLine pptokens ""
     putStrLn ""
     putStrLn "Pretty print:"
     putStrLn $ (prettyPrint 25 "  " testString) ""
     case maybeFileName of
       Just fileName ->
         do putStrLn ""
            putStrLn $ "File name: '" ++ fileName ++ "'"
            content <- readFile fileName
            putStrLn $ "Contents: '''\\\n" ++ content ++ "'''"
            do let contentTokens = alexScanTokens2 content
                   contentPPTokens = toPPTokens "  " contentTokens [SepClean] []
               putStrLn ""
               putStrLn "PPTokens:"
               putStrLn $ showListWithNewLine contentPPTokens ""
               putStrLn ""
               putStrLn "Pretty print:"
               putStrLn $ (prettyPrint 120 "  " content) ""
            return ()
       _ -> return ()

showWithNewLine :: (Show a) => a -> ShowS
showWithNewLine x = shows x . (showString "\n")

showListWithNewLine :: (Show a) => [a] -> ShowS
showListWithNewLine xs = showListWith showWithNewLine xs

-- EOF