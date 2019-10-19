
module Data.PrettyPrint.Swallow where

import Data.Sequence (ViewL((:<)), ViewR((:>)), (|>))
import Text.PrettyPrint ((<>), (<+>))
import Text.Show (showListWith)
import qualified Data.Foldable as Fold (toList)
import qualified Data.Sequence as Seq (Seq, empty, singleton, viewl, viewr)
import qualified Text.PrettyPrint as PP
  ( Doc
  , Mode(..)
  , TextDetails(..)
  , cat
  , char
  , empty
  , fullRender
  , hang
  , nest
  , sep
  , text
  )

import Data.PrettyPrint.Parens
  ( Token(T, theType,theValue)
  , TokenType(..)
  , alexScanTokens2
  )

data Chunks
 = Chunks
   { theParent :: Maybe Chunks
   , theWords :: Seq.Seq PP.Doc
   , theFragments :: Seq.Seq PP.Doc
   , theAddIndentationFlag :: !Bool
   }

createChunk :: Maybe Chunks -> Chunks
createChunk maybeChunks = Chunks maybeChunks Seq.empty Seq.empty True

pushChunk :: Bool -> Chunks -> Chunks
pushChunk addIndentation chunks = (createChunk $ Just chunks) { theAddIndentationFlag = addIndentation }

popChunk :: Int -> Chunks -> Chunks
popChunk indent chunks =
  let lastWord = PP.cat $ Fold.toList $ theFragments chunks
      allWords = (theWords chunks |> lastWord)
      unindented =
        case Seq.viewl allWords of
          w :< ws -> PP.hang w indent (PP.sep $ Fold.toList ws)
      thisDoc =
        if theAddIndentationFlag chunks
          then PP.nest indent unindented
          else unindented
  in case theParent chunks of
       Just parent -> parent { theFragments = (theFragments parent |> thisDoc) }
       _ -> emptyChunk { theFragments = Seq.singleton $ thisDoc }

emptyChunk = createChunk Nothing

showWithNewLine :: (Show a) => a -> ShowS
showWithNewLine x = shows x . (showString "\n")

showListWithNewLine :: (Show a) => [a] -> ShowS
showListWithNewLine xs = showListWith showWithNewLine xs

prettyPrint :: Int -> Int -> String -> ShowS
prettyPrint lineWidth indent str =
  let tokens = alexScanTokens2 str
  in prettyPrintTokens lineWidth indent tokens

prettyPrintTokens :: Int -> Int -> [Token] -> ShowS
prettyPrintTokens lineWidth indent tokens = fullPrettyPrint lineWidth indent tokens prependShowS id

prependShowS :: PP.TextDetails -> ShowS -> ShowS
prependShowS (PP.Chr ch) s = (showChar ch) . s
prependShowS (PP.Str str) s = (showString str) . s
prependShowS (PP.PStr str) s = (showString str) . s

fullPrettyPrint :: Int -> Int -> [Token] -> (PP.TextDetails -> a -> a) -> a -> a
fullPrettyPrint lineWidth indent tokens prepend final =
  let ppTokens = toPPTokens 1 tokens [SepClean]  []
      chunks = ppTokensToChunks indent ppTokens emptyChunk
      doc = collapse indent chunks
  in PP.fullRender PP.PageMode lineWidth 1.5 prepend final doc

collapse :: Int -> Chunks -> PP.Doc
collapse indent chunks@(Chunks (Just _) _ _ _) = collapse indent $ popChunk indent chunks
collapse indent chunks@(Chunks Nothing _ _ _) =
  let fragments = theFragments $ popChunk 0 (chunks { theParent = Just emptyChunk })
      rightView = Seq.viewr fragments
  in case rightView of
       _ :> doc -> doc
       _ -> PP.empty 

addFragment :: String -> Chunks -> Chunks
addFragment str oldChunks =
  let oldFragments = theFragments oldChunks
      newFragments = oldFragments |> (PP.text str)
  in oldChunks { theFragments = newFragments }

tokensToChunks :: Int -> [Token] -> Chunks -> Chunks
tokensToChunks _ [] chunks = chunks
tokensToChunks indent ((T Space _ _) : ts) oldChunks@(Chunks _ oldWords oldFragments _) =
  let lastWord = PP.cat $ Fold.toList oldFragments
      newWords = oldWords |> lastWord
  in tokensToChunks indent ts (oldChunks { theWords = newWords, theFragments = Seq.empty })
tokensToChunks indent ((T Open _ str) : ts) oldChunks@(Chunks _ _ oldFragments _) =
  tokensToChunks indent ts $ pushChunk True $ addFragment str oldChunks
tokensToChunks indent ((T Close _ str) : ts) oldChunks@(Chunks _ _ oldFragments _) =
  tokensToChunks indent ts $ addFragment str $ popChunk indent oldChunks
tokensToChunks indent ((T _ _ str) : ts) oldChunks =
  tokensToChunks indent ts $ addFragment str oldChunks

data PPToken
  = PToken String
  | PBreak String String String -- unbroken, pre-break, post-break
  | POpen Int
  | PClose
  | PEOT
  deriving (Eq, Show)

ppTokensToChunks :: Int -> [PPToken] -> Chunks -> Chunks
ppTokensToChunks _ [] chunks = chunks
ppTokensToChunks indent ((PBreak _ _ _) : ts) oldChunks@(Chunks _ oldWords oldFragments _) =
  let lastWord = PP.cat $ Fold.toList oldFragments
      newWords = oldWords |> lastWord
  in ppTokensToChunks indent ts (oldChunks { theWords = newWords, theFragments = Seq.empty })
ppTokensToChunks indent ((POpen delta) : ts) oldChunks@(Chunks _ _ oldFragments _) =
  ppTokensToChunks indent ts $ pushChunk (delta > 0) oldChunks
ppTokensToChunks indent (PClose : ts) oldChunks@(Chunks _ _ oldFragments _) =
  ppTokensToChunks indent ts $ popChunk indent oldChunks
ppTokensToChunks indent ((PToken str) : ts) oldChunks =
  ppTokensToChunks indent ts $ addFragment str oldChunks
ppTokensToChunks indent (PEOT : _) oldChunks = oldChunks

toPToken :: Token -> PPToken

toPToken token =
  case theType token of
    Space -> PBreak (theValue token) "" ""
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

toPPTokens :: Int -> [Token] -> [SepState] -> [PPToken] -> [PPToken]

toPPTokens indent tokens sepStateStack [] = toPPTokens indent tokens sepStateStack [PEOT]

toPPTokens indent [] (SepClean : sss) pptokens@(_:_) = toPPTokens indent [] sss pptokens
toPPTokens indent [] (SepReady _ : sss) pptokens@(_:_) = toPPTokens indent [] sss (addTokens [PClose] pptokens)
toPPTokens indent [] (SepOpen : sss) pptokens@(_:_) = toPPTokens indent [] sss (addTokens [PClose, PClose] pptokens)
toPPTokens _ [] [] pptokens@(_:_) = pptokens

toPPTokens indent (token : tokens) sepStateStack pptokens@(_:_) =
  let (newSepStateStack, sepPTokens) = updateSepStack indent sepStateStack $ theType token
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

updateSepStack :: Int -> [SepState] -> TokenType -> ([SepState], [PPToken])

updateSepStack _ (SepReady _ : sss) Open = (SepClean : (SepClean : sss), [PClose])
updateSepStack _ (SepOpen : sss) Open = (SepClean : (SepClean : sss), [PClose, PClose])
updateSepStack _ sss Open = (SepClean : sss, [])
updateSepStack indent (SepOpen : sss) Close = updateSepStackClose indent sss [PClose, PClose]
updateSepStack indent (SepReady _ : sss) Close = updateSepStackClose indent sss [PClose]
updateSepStack indent (SepClean : sss) Close = updateSepStackClose indent sss []
updateSepStack _ (SepOpen : sss) Sep = (SepClean : sss, [PClose, PClose])
updateSepStack _ (_ : sss) Sep = (SepClean : sss, [PClose])
updateSepStack _ (SepClean : sss) Word = (SepReady False : sss, [POpen 0])
updateSepStack _ (SepClean : sss) Quoted = (SepReady False : sss, [POpen 0])
updateSepStack indent (SepReady True : sss) Word = (SepReady False : sss, [])
updateSepStack indent (SepReady True : sss) Quoted = (SepReady False : sss, [])
updateSepStack indent (SepReady False : sss) Word = (SepOpen : sss, [POpen indent])
updateSepStack indent (SepReady False : sss) Quoted = (SepOpen : sss, [POpen indent])
updateSepStack _ sss _ = (sss, [])

updateSepStackClose :: Int -> [SepState] -> [PPToken] -> ([SepState], [PPToken])

updateSepStackClose _ (SepClean : sss) tokens = (SepReady True : sss, tokens ++ [POpen 0])
updateSepStackClose indent (SepReady _ : sss) tokens = (SepOpen : sss, tokens ++ [POpen indent])
updateSepStackClose indent sss tokens = (sss, tokens)

test :: Maybe String -> IO ()
test maybeFileName =
  do let testString = "try { for (int i = 1; i < 10; i++) { printf(\"i: %d\", i); [] } } catch (Exception with more slow stuff ex) { ExceptionWrapper.wrap(ex); } <?xml lang='en' ?> "
         tokens = alexScanTokens2 testString
         ppTokens = toPPTokens 1 tokens [SepClean] []
     putStrLn ""
     putStrLn "Tokens"
     putStrLn $ showListWithNewLine tokens ""
     putStrLn ""
     putStrLn "PPTokens"
     putStrLn $ showListWithNewLine ppTokens ""
     putStrLn ""
     putStrLn "Formatted"
     putStrLn $ (prettyPrintTokens 150 2 tokens) ""
     case maybeFileName of
       Just fileName ->
         do putStrLn ""
            putStrLn $ "File: [" ++ fileName ++ "]"
            contents <- readFile fileName
            putStrLn $ prettyPrint 80 2 contents ""
       _ -> return ()

--EOF