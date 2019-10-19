{-# LANGUAGE FlexibleContexts #-}
-- |Concurrent implementation of the LExAu pipeline.
module LExAu.Pipeline.Concurrent (PipelineContext(..), runPipeline, test) where

import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, readMVar, swapMVar, tryTakeMVar)
import Control.Exception (SomeException, bracket, evaluate, mapException, try)
import Control.Monad (liftM)
import Control.Monad.ST (RealWorld, ST, runST, stToIO)
import Data.List (foldl')
import Data.Maybe (isJust)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import System.Directory(doesFileExist, removeFile, renameFile)
import System.IO (Handle, IOMode(WriteMode), hClose, hPutStr, withFile)
import System.Random (RandomGen(split), getStdGen)

import LExAu.API.Alphabet (
    Alphabet,
    DirectedSymbol(DirectedSymbol),
    SymbolDirection(Action,Response)
  )
import LExAu.API.DescriptionLength (DescriptionLength(descriptionLength))
import LExAu.API.Distribution (DistributionFactory(DistributionFactory))
import LExAu.API.Encounter (
    Encounter(stepIO),
    RecordingEncounter(clearEnvironmentHistory,environmentHistory)
  )
import LExAu.API.Indexed (member)
import LExAu.API.Interaction (
    Imitator(clearUpdated),
    Interaction(offerDirectedSymbol),
    Policy,
    offerDirectedSymbolST
  )
import LExAu.API.Model
  ( AlphabetUpdate(..)
  , EvaluateUpdate(..)
  , HasCheckSum(checkSum)
  , ModelOfExpectedBehavior
  , OptimizeUpdate(..)
  , PolicyUpdate(..)
  , Updater(update)
  , UpdaterIO(updateIO)
  , pureUpdateToIO
  )
import LExAu.API.Named (Named(name))
import LExAu.Distribution.Histogram (histogramFromList)
import LExAu.Model.Markov
  ( createSimpleMarkovPolicy
  )
import LExAu.Utilities.Logging
  ( Level(..)
  , LogFunction
  , Loggable(logs)
  , logFunction
  , logSilent
  , logVerbose
  )
import LExAu.Utilities.ThreadManager
  ( ThreadManager
  , ThreadStatus(..)
  , allThreadIds
  , forkLoggingManaged
  , forkNamedManaged
  , getMainLogger
  , getNamedStatus
  , getStatus
  , logErrorOnAbort
  , newManager
  , stopLogger
  , waitAll
  )
import qualified LExAu.API.Encounter as Encounter (Encounter(stepCount))
import qualified LExAu.API.Interaction as Interaction (Source(stepCount))
import qualified LExAu.IO.Example as Example (darkResponse, exampleAlphabet, exampleEncounter, leftAction, lightResponse, rightAction)
import qualified LExAu.Model.HistoryTree as HistoryTree
  ( createEmptyHistoryTreeExpected
  , createEmptyHistoryTreeProcess
  , getModelOfObservedBehavior
  , getUpdateSteps
  )
import qualified LExAu.Utilities.MVar as MM
  ( EntryContext
  , newEntryContext
  , putMVar
  , readMVar
  , takeMVar
  , tryTakeMVar
  )

data SymbolMessageImpl symbolType = EndOfSymbols | SendFeedback | Symbol(symbolType)

instance (Loggable symbolType) => Loggable (SymbolMessageImpl symbolType) where
  logs EndOfSymbols = showString "EndOfSymbols"
  logs SendFeedback = showString "SendFeedback"
  logs (Symbol symbol) = logs symbol

data EncounterPortsImpl alphabetType symbolType policyType =
  EncounterPortsImpl {
    alphabetPort :: MVar alphabetType,
    symbolsChannel :: Chan (SymbolMessageImpl (DirectedSymbol symbolType)),
    feedbackChannel :: Chan (),
    policyPort :: MVar (Maybe policyType)
  }

createEncounterPorts :: alphabetType -> policyType -> IO (EncounterPortsImpl alphabetType symbolType policyType)
createEncounterPorts startAlphabet startPolicy =
  do alphabetMVar <- newMVar startAlphabet
     symbolsChan <- newChan
     feedbackChan <- newChan
     policyMVar <- newMVar $ Just startPolicy
     return $ EncounterPortsImpl alphabetMVar symbolsChan feedbackChan policyMVar

data RecorderPortsImpl processType alphabetType symbolType =
  RecorderPortsImpl {
    recorderAlphabetPort :: MVar alphabetType,
    recorderSymbolsChannel :: Chan (SymbolMessageImpl (DirectedSymbol symbolType)),
    recorderFeedbackChannel :: Chan (),
    recorderPublishUpdate :: (processType -> IO ()),
    recorderPrepareForUpdate :: IO Bool
  }

createRecorderPorts :: STRef RealWorld MM.EntryContext -> (EncounterPortsImpl alphabetType symbolType policyType) -> MVar (Maybe expectedModelUpdateType) -> (processType -> expectedModelUpdateType) -> RecorderPortsImpl processType alphabetType symbolType
createRecorderPorts contextRef (EncounterPortsImpl alphabetMVar symbolsChan feedbackChan _) observedMVar updateExtractor =
  RecorderPortsImpl
    alphabetMVar symbolsChan feedbackChan
    (publishUpdateOfObservedModel contextRef updateExtractor observedMVar)
    (prepareForUpdateOfObservedModel observedMVar)

publishUpdateOfObservedModel :: STRef RealWorld MM.EntryContext -> (processType -> expectedModelUpdateType) -> MVar (Maybe expectedModelUpdateType) -> processType -> IO ()

publishUpdateOfObservedModel contextRef updateExtractor observedMVar process =
  publishUpdate contextRef (showString "Observed model") observedMVar (updateExtractor process)

prepareForUpdateOfObservedModel :: MVar (Maybe expectedModelUpdateType) -> IO Bool

prepareForUpdateOfObservedModel observedMVar = isJust `fmap` tryTakeMVar observedMVar

interactAndPublishUntilNothing :: (
      Encounter encounterType policyType alphabetType symbolType,
      Updater PolicyUpdate encounterType policyType,
      Loggable symbolType
    ) =>
  LogFunction -> String -> EncounterPortsImpl alphabetType symbolType policyType -> STRef RealWorld encounterType -> Maybe Integer -> IO ()

interactAndPublishUntilNothing logger label publishPort encounterRef maybeLimit =
  do (continue, newLimit) <- return $
       case maybeLimit of
         (Just limit) ->
           if limit > 0
             then (True, Just $ limit - 1)
             else (False, Just $ 0)
         Nothing -> (True, Nothing)
     if continue
       then
         do (maybeResponse, maybeAction) <- stepToChanIO logger encounterRef publishPort
            logger "Generated symbols" [showString label, logs maybeResponse, logs maybeAction]
            case (maybeResponse, maybeAction) of
              (Nothing, Nothing) -> return ()
              _ -> interactAndPublishUntilNothing logger label publishPort encounterRef newLimit
       else return ()

stepToChanIO :: (
      Encounter encounterType policyType alphabetType symbolType,
      Updater PolicyUpdate encounterType policyType
    ) =>
  LogFunction -> STRef RealWorld encounterType -> EncounterPortsImpl alphabetType symbolType policyType -> IO (Maybe symbolType, Maybe symbolType)

stepToChanIO logger encounterRef (EncounterPortsImpl alphabetMVar channel feedback policyUpdates) =
  do maybeUpdateEncounter encounterRef policyUpdates
     (maybeResponse, maybeAction, maybeNewAlphabet) <- stepIO encounterRef
     case (maybeNewAlphabet) of
       Nothing -> return ()
       Just newAlphabet ->
         do logger "Publish new alphabet" [shows newAlphabet]
            swapMVar alphabetMVar newAlphabet
            return ()
     encounter <- stToIO $ readSTRef encounterRef
     case (maybeResponse, maybeAction) of
       (Nothing, Nothing) -> writeChan channel EndOfSymbols
       _ ->
         do maybeWriteChan channel Response maybeResponse
            maybeWriteChan channel Action maybeAction
     logger "Sent" [shows $ name `fmap` maybeResponse, shows $ name `fmap` maybeAction]
     numberOfSteps <- return $ Encounter.stepCount encounter
     if numberOfSteps `rem` 10000 == 0
       then do logger "Step" [shows numberOfSteps, showString "..."]
               writeChan channel SendFeedback
               _ <- readChan feedback
               logger "... continue." []
       else return ()
     return $! (maybeResponse, maybeAction)

maybeUpdateEncounter :: (
      Updater PolicyUpdate encounterType policyType
    ) =>
  STRef RealWorld encounterType -> MVar (Maybe policyType) -> IO Bool
maybeUpdateEncounter encounterRef policyUpdates =
  do maybeMaybePolicyUpdate <- tryTakeMVar policyUpdates
     case maybeMaybePolicyUpdate of
       Just maybePolicyUpdate ->
         do case maybePolicyUpdate of
              Just policyUpdate ->
                do oldEncounter <- stToIO $ readSTRef encounterRef
                   maybeUpdatedEncounter <- return $ update PolicyUpdate policyUpdate oldEncounter
                   case maybeUpdatedEncounter of
                     Just updatedEncounter ->
                       do stToIO $ writeSTRef encounterRef updatedEncounter
                          return True -- Updated the encounter
                     Nothing -> return True -- Update did not return a new encounter
              Nothing -> return False -- Received 'Nothing', so stop
       Nothing -> return True -- No update received

maybeWriteChan :: (Alphabet alphabetType symbolType) => Chan (SymbolMessageImpl (DirectedSymbol symbolType)) -> SymbolDirection -> Maybe symbolType -> IO ()
maybeWriteChan channel direction maybeSymbol =
  case maybeSymbol of
    Just symbol -> writeChan channel $! Symbol $! DirectedSymbol direction symbol
    Nothing -> return ()

readAndRecordUntilEndOfSymbols :: (
    Interaction processType alphabetType symbolType,
    Updater AlphabetUpdate processType alphabetType,
    Alphabet alphabetType symbolType,
    Loggable processType,
    Loggable (DirectedSymbol symbolType)
  ) =>
   LogFunction -> RecorderPortsImpl processType alphabetType symbolType -> processType -> (processType -> DirectedSymbol symbolType -> processType) -> IO processType

readAndRecordUntilEndOfSymbols logger recorderPorts startModel recorderFunction =
  do symbolOrCommand <- readChan $ recorderSymbolsChannel recorderPorts
     logger "Received symbol" [logs symbolOrCommand]
     numberOfSteps <- return $ (Interaction.stepCount startModel)
     if numberOfSteps `rem` 20000 == 0
       then do logger "Recorder step" [shows numberOfSteps]
               logger "Model" [logs startModel]
       else return ()
     case symbolOrCommand of
       (Symbol symbol) ->
         do newAlphabet <- readMVar $ recorderAlphabetPort recorderPorts
            updatedModel <- return $
              case update AlphabetUpdate newAlphabet startModel of
                Nothing -> startModel
                Just theUpdatedModel -> theUpdatedModel
            continueOldUpdate <- recorderPrepareForUpdate recorderPorts
            correctlyMarkedModel <- return $
              if continueOldUpdate
                then updatedModel
                else clearUpdated updatedModel
            nextModel <- return $ recorderFunction correctlyMarkedModel symbol
            (recorderPublishUpdate recorderPorts) nextModel
            readAndRecordUntilEndOfSymbols logger recorderPorts nextModel recorderFunction
       SendFeedback ->
         do writeChan (recorderFeedbackChannel recorderPorts) ()
            readAndRecordUntilEndOfSymbols logger recorderPorts startModel recorderFunction
       EndOfSymbols -> return $ startModel

publishUpdate :: STRef RealWorld MM.EntryContext -> ShowS -> MVar (Maybe updateType) -> updateType -> IO ()
publishUpdate contextRef id port update =
  do oldUpdate <- tryTakeMVar port
     MM.putMVar contextRef (showString "Publish update: " . id) port (Just update)

data SerializerContext objectType =
  SerializerContext
    { theFileNamePrefix :: String
    , theLastId :: Int
    , theSerializer :: objectType -> ShowS
    }

data LogContext updateType modelType =
  LogContext
    { theLogger :: LogFunction
    , theLabel :: String
    , theSerializedDir :: Maybe String
    , theUpdateSerializerContext :: Maybe (SerializerContext updateType)
    , theResultSerializerContext :: Maybe (SerializerContext modelType)
    , theEntryContext :: STRef RealWorld MM.EntryContext
    }

serializeObject
  :: LogContext updateType modelType
  -> objectType
  -> Maybe (SerializerContext objectType)
  -> IO (Maybe (SerializerContext objectType))

serializeObject logContext object maybeSerializerContext =
  let logger = theLogger logContext
      maybeSerializedDir = theSerializedDir logContext
  in case (maybeSerializerContext, maybeSerializedDir) of
       (Just serializerContext, Just serializedDir) ->
         do let fileNamePrefix = theFileNamePrefix serializerContext
                lastId = theLastId serializerContext
                thisId = (lastId `mod` 3) + 1
                exportFileName = serializedDir ++ "/" ++ fileNamePrefix ++ "-" ++ (show thisId) ++ ".dat"
                tmpFileName = exportFileName ++ "~"
                serializer = theSerializer serializerContext
                writer handle = hPutStr handle $ (serializer object) ""
            logger "Export file" [shows exportFileName]
            exportFileExists <- doesFileExist exportFileName
            if exportFileExists
              then removeFile exportFileName
              else return ()
            withFile tmpFileName WriteMode writer
            renameFile tmpFileName exportFileName
            return $ Just $ serializerContext { theLastId = thisId }
       _ -> return Nothing

updateStep ::
  ( HasCheckSum modelType
  , Loggable modelType
  )
  => (updateStepType -> modelType -> IO (Maybe modelType))
  -> updateStepType
  -> (modelType, Bool, LogContext updateType modelType)
  -> IO (modelType, Bool, LogContext updateType modelType)

updateStep updateStepFunction updateStep startTuple@(startModel, startDirty, logContext) =
  do let logger = theLogger logContext
         label = theLabel logContext
         theCheckSum maybeModel = maybe 0 checkSum maybeModel
         logException exception = logger "Exception" [showString label, shows (exception :: SomeException), logs startModel] >> return Nothing
     maybeUpdatedModel <- updateStepFunction updateStep startModel
     resultOrException <- try $ evaluate $ seq (theCheckSum maybeUpdatedModel) maybeUpdatedModel
     maybeNextModel <- either logException return resultOrException
     -- logger "Maybe updated model" [showString label, logs maybeNextModel]
     case maybeNextModel of
       Nothing -> return $ startTuple
       Just nextModel ->
         do logger "Serialize..." []
            maybeNewResultSerializerContext <- serializeObject logContext nextModel $ theResultSerializerContext logContext
            logger "Serialized." []
            return $
              case maybeNewResultSerializerContext of
                Nothing -> (nextModel, True, logContext)
                Just newResultSerializerContext ->
                  ( nextModel
                  , True
                  , logContext { theResultSerializerContext = maybeNewResultSerializerContext }
                  )

consumeAndUpdateUntilNothing :: (
      HasCheckSum modelType,
      Loggable modelType,
      Loggable updateType
    ) =>
  LogContext updateType modelType -> MVar (Maybe updateType) -> MVar (Maybe modelType) -> modelType -> (updateType -> [updateStepType]) -> (updateStepType -> modelType -> IO (Maybe modelType)) -> IO modelType

consumeAndUpdateUntilNothing logContext consumePort publishPort startModel updateStepper updateStepFunction =
  do let logger = theLogger logContext
         label = theLabel logContext
         contextRef = theEntryContext logContext
     maybeUpdate <- MM.takeMVar contextRef (showString label . showString ": Consume update") consumePort
     case maybeUpdate of
       (Just update) ->
         do let updateSteps = updateStepper update
            -- logger "Received update" [showString label, logs update]
            tmpLogContext <-
              do maybeNewUpdateSerializerContext <- serializeObject logContext update $ theUpdateSerializerContext logContext
                 return $
                   case maybeNewUpdateSerializerContext of
                     Nothing -> logContext
                     Just newUpdateSerializerContext -> logContext { theUpdateSerializerContext = maybeNewUpdateSerializerContext }
            (theNextModel, isModified, nextLogContext) <- foldl' (>>=) (return (startModel, False, tmpLogContext)) $ map (updateStep updateStepFunction) updateSteps
            if isModified
              then
                -- do logger "Next model" [showString label, logs theNextModel]
                   publishUpdate contextRef (showString label) publishPort theNextModel
              else return ()
            consumeAndUpdateUntilNothing nextLogContext consumePort publishPort theNextModel updateStepper updateStepFunction
       Nothing -> 
         do logger "Received <Nothing>" []
            return $ startModel

maybeTake :: Maybe Int -> [a] -> [a]
maybeTake maybeNumber list =
  case maybeNumber of
    Nothing -> list
    (Just number) -> take number list

data PipelineContext updateType modelOfExpectedBehaviorType =
  PipelineContext
    { pcTestEncounterOnly :: Bool
    , pcStepLimit :: Maybe Integer
    , pcUpdateSerializer :: Maybe (updateType -> ShowS)
    , pcExpectedSerializer :: Maybe (modelOfExpectedBehaviorType -> ShowS)
    , pcSerializedDir :: Maybe String
    }

finalizeMVar :: String -> STRef RealWorld MM.EntryContext -> MVar (Maybe a) -> IO ()
finalizeMVar id contextRef mvar =
  do maybeMaybeValue <- MM.tryTakeMVar contextRef (showString "Finalize " . showString id . showString "*") mvar
     case maybeMaybeValue of
       Just maybeValue ->
         case maybeValue of
           Just value ->
             do MM.putMVar contextRef (showString "Finalize " . showString id . showString "+") mvar maybeValue
                MM.putMVar contextRef (showString "Finalize " . showString id . showString "-") mvar Nothing
           _ -> return ()
       Nothing -> return ()
     MM.putMVar contextRef (showString "Finalize") mvar Nothing

-- |Creates ports for the communication between the given components and runs
-- the complete system. 
runPipeline :: (
      Encounter encounterType policyType alphabetType symbolType,
      Interaction modelOfObservedBehaviorType alphabetType symbolType,
      Updater PolicyUpdate encounterType policyType,
      Updater AlphabetUpdate modelOfObservedBehaviorType alphabetType,
      UpdaterIO OptimizeUpdate modelOfExpectedBehaviorType updateType,
      UpdaterIO EvaluateUpdate policyType modelOfExpectedBehaviorType,
      HasCheckSum policyType,
      HasCheckSum modelOfExpectedBehaviorType,
      Loggable symbolType,
      Loggable (DirectedSymbol symbolType),
      Loggable policyType,
      Loggable modelOfObservedBehaviorType,
      Loggable modelOfExpectedBehaviorType,
      Loggable updateType
    ) =>
  alphabetType
    -> encounterType
    -> policyType
    -> modelOfObservedBehaviorType
    -> (modelOfObservedBehaviorType -> [updateType])
    -> modelOfExpectedBehaviorType
    -> PipelineContext updateType modelOfExpectedBehaviorType
    -> IO (encounterType, Maybe modelOfObservedBehaviorType)

runPipeline startAlphabet startEncounter startPolicy startModelOfObservedBehavior updateExtractor startModelOfExpectedBehavior pipelineContext =
  bracket
    newManager
    stopLogger
    (\ threadManager ->
      do let testEncounterOnly = pcTestEncounterOnly pipelineContext
             maybeStepLimit = pcStepLimit pipelineContext
             maybeSerializer = pcExpectedSerializer pipelineContext
             maybeExportDir = pcSerializedDir pipelineContext
         mainLogger <- return $ logFunction FINE (getMainLogger threadManager) "[MAIN]"
         mainLogger "Run pipeline" []
         stdGenMVar <- (getStdGen >>= newMVar)
         mainEntryContextRef <- MM.newEntryContext stdGenMVar mainLogger
         mainEntryContextSilentRef <- MM.newEntryContext stdGenMVar logSilent
         encounterPorts <- createEncounterPorts startAlphabet startPolicy
         observedPort <- newEmptyMVar
         recorderPorts <- return $ createRecorderPorts mainEntryContextSilentRef encounterPorts observedPort updateExtractor
         expectedPort <- newEmptyMVar
         writeChan (feedbackChannel encounterPorts) ()
         strategyPort <- return $ policyPort encounterPorts
         stateMVar <- newEmptyMVar

         -- Fork thread for Encounter
         forkLoggingManaged threadManager "Encounter"
           (\ logWrapper ->
              do let logger = logFunction FINE logWrapper
                 encounterRef <- stToIO $ newSTRef startEncounter
                 interactAndPublishUntilNothing logSilent "(Encounter)" encounterPorts encounterRef maybeStepLimit
                 writeChan (symbolsChannel encounterPorts) EndOfSymbols
                 encounterNextState <- stToIO $ readSTRef encounterRef
                 MM.putMVar mainEntryContextRef (showString "Run pipeline: Publish next state") stateMVar $! encounterNextState
           ) logErrorOnAbort
         maybeNextModelOfObservedBehavior <-
           if testEncounterOnly
             then return $ Nothing
             else
               do -- Fork thread for Optimizer
                  optimizerThreadID <- forkLoggingManaged threadManager "Optimizer"
                    (\ logWrapper ->
                       do let logger = logFunction FINE logWrapper "[Optimizer]"
                              maybeExpectedSerializerContext = liftM (SerializerContext "expected" 0) $ pcExpectedSerializer pipelineContext
                          entryContextRef <- MM.newEntryContext stdGenMVar logger
                          logContext <- return $ LogContext logger "'Optimizer'" maybeExportDir Nothing maybeExpectedSerializerContext entryContextRef
                          expectedNextState <- consumeAndUpdateUntilNothing logContext observedPort expectedPort startModelOfExpectedBehavior id $ updateIO OptimizeUpdate
                          logger "Next model of expected behavior" [logs expectedNextState]
                          return ()
                    )
                    (\ h es ->
                       do logErrorOnAbort h es
                          entryContextRef <- MM.newEntryContext stdGenMVar h
                          finalizeMVar "expected port" entryContextRef expectedPort
                    )

                  -- Fork thread for Evaluator
                  evaluatorThreadID <- forkLoggingManaged threadManager "Evaluator"
                    (\ logWrapper ->
                       do let logger = logFunction FINE logWrapper "[Evaluator]"
                          entryContextRef <- MM.newEntryContext stdGenMVar logger
                          logContext <- return $ LogContext logger "'Evaluator'" Nothing Nothing Nothing entryContextRef
                          policyNextState <- consumeAndUpdateUntilNothing logContext expectedPort strategyPort startPolicy (:[]) $ updateIO EvaluateUpdate
                          logger "Next strategy" [logs policyNextState]
                          return ()
                    )
                    (\ h es ->
                       do logErrorOnAbort h es
                          entryContextRef <- MM.newEntryContext stdGenMVar h
                          finalizeMVar "strategy port" entryContextRef strategyPort
                    )

                  -- Run the Recorder
                  recorderNextState <- readAndRecordUntilEndOfSymbols logSilent recorderPorts startModelOfObservedBehavior offerDirectedSymbol
                  mainLogger "Sending end of run to Optimizer and Evaluator" []
                  showThreads mainLogger threadManager

                  maybeFinalUpdate <- MM.readMVar mainEntryContextRef (showString "Get final update") observedPort
                  case (maybeFinalUpdate, pcUpdateSerializer pipelineContext) of
                    (Just finalUpdate, Just updateSerializer) ->
                      do let serializeList serialize [] = id
                             serializeList serialize (x : xs) = serialize x . serializeList serialize xs
                         serializeObject
                           (LogContext logVerbose "" Nothing Nothing Nothing mainEntryContextRef)
                           finalUpdate
                           (Just $ SerializerContext "observed" 0 (serializeList updateSerializer))
                         return ()
                    (Just finalUpdate, Nothing) -> mainLogger "No serializer to save final updates" [shows $ length finalUpdate]
                    (Nothing, _) -> mainLogger "No final update" []

                  optmizerStatus <- getStatus threadManager optimizerThreadID
                  case optmizerStatus of
                    Just Running -> finalizeMVar "observed port(M)" mainEntryContextRef observedPort
                    _ -> return ()

                  evaluatorStatus <- getStatus threadManager evaluatorThreadID
                  case evaluatorStatus of
                    Just Running -> finalizeMVar "expected port(M)" mainEntryContextRef expectedPort
                    _ -> return ()

                  mainLogger "----------" []
                  mainLogger "Resulting model of observed behavior" []
                  mainLogger "Recorder next state" [logs recorderNextState]
                  return $ Just recorderNextState
         showThreads mainLogger threadManager
         waitAll threadManager
         encounterNextState <- MM.takeMVar mainEntryContextRef (showString "Run pipeline: Get encounter next state") stateMVar
         return (encounterNextState, maybeNextModelOfObservedBehavior)
    )

showThreadStatus logger threadManager threadId =
  do maybeStatus <- getNamedStatus threadManager threadId
     case maybeStatus of
       Just status ->
         logger "Thread" $ [shows threadId, shows $ fst status, shows $ snd status]
       Nothing ->
         logger "Thread" $ [shows threadId, showString "Nothing"]
     return ()

showThreads logger threadManager =
  do threadIds <- allThreadIds threadManager
     logger "Thread IDs" $ map shows threadIds
     foldr (>>) (return ()) $ map (showThreadStatus logger threadManager) threadIds

-- |Tests the Concurrent Pipeline implementation.
test :: Bool -> Maybe Integer -> Maybe String -> IO ()
test testEncounterOnly maybeStepLimit maybeOptimizerCacheLocation = do
  let factory = DistributionFactory $ Just histogramFromList
      pipelineContext = PipelineContext testEncounterOnly maybeStepLimit Nothing Nothing Nothing

  putStrLn $ ""
  putStrLn $ "Test combination of modules"
  logVerbose "Step limit" [shows maybeStepLimit]

  stdGen <- getStdGen
  (stdGen1, stdGen2a) <- return $ split stdGen
  (stdGen2, stdGen3) <- return $ split stdGen2a

  putStrLn ""
  policy <- return $ createSimpleMarkovPolicy factory Example.exampleAlphabet [Example.leftAction, Example.rightAction] stdGen3
  logVerbose "Policy" [logs policy]

  encounterStartState <- return $ Example.exampleEncounter stdGen1
  recorderStartState <- return $ HistoryTree.createEmptyHistoryTreeProcess Example.exampleAlphabet factory stdGen2
  expectedStartState <- return $ HistoryTree.createEmptyHistoryTreeExpected Example.exampleAlphabet factory maybeOptimizerCacheLocation
  (encounterNextState, maybeRecorderNextState) <- runPipeline Example.exampleAlphabet encounterStartState policy recorderStartState (HistoryTree.getUpdateSteps 100) expectedStartState pipelineContext

  (encounterEndState, maybeRecorderEndState) <- case maybeRecorderNextState of
    Nothing -> return (encounterNextState, Nothing)
    (Just recorderNextState) ->
      do -- Fork another Encounter thread
         threadManager <- newManager
         encounterPorts <- createEncounterPorts Example.exampleAlphabet policy
         stateMVar <- newEmptyMVar
         observedPort <- newEmptyMVar
         expectedPort <- newEmptyMVar
         stdGenMVar <- (getStdGen >>= newMVar)
         testEntryContextRef <- MM.newEntryContext stdGenMVar logVerbose
         recorderPorts <- return $ createRecorderPorts testEntryContextRef encounterPorts observedPort HistoryTree.getModelOfObservedBehavior
         forkNamedManaged threadManager "Encounter2" $
           do encounterRef <- stToIO $ newSTRef (clearEnvironmentHistory encounterNextState True)
              interactAndPublishUntilNothing logVerbose "Encounter2" encounterPorts encounterRef (Just 20)
              encounterNextState <- stToIO $ readSTRef encounterRef
              writeChan (symbolsChannel encounterPorts) EndOfSymbols
              MM.putMVar testEntryContextRef (showString "Test: Publish next state") stateMVar encounterNextState

         -- Run the Recorder again
         recorderLastState <- readAndRecordUntilEndOfSymbols logVerbose recorderPorts recorderNextState offerDirectedSymbol

         encounterLastState <- MM.takeMVar testEntryContextRef (showString "Test: Get entcounter last state") stateMVar
         waitAll threadManager
         return $ (encounterLastState, Just recorderLastState)

  putStrLn $ "Extra symbols"
  history <- return $ environmentHistory encounterEndState
  print $ map name history
  print $ Encounter.stepCount encounterEndState

  case maybeRecorderEndState of
    (Just recorderEndState) ->
      do putStrLn $ "Concurrently updated model of observed behavior"
         logVerbose "Recorder end state" [logs recorderEndState]

         recorderCleanState <- return $ clearUpdated recorderEndState
         putStrLn ""
         putStrLn $ "Model of observed behavior with all updated flags cleared"
         logVerbose "Recorder clean state" [logs recorderCleanState]

         recorderDirtyState <- return $ runST (
             do let darkResponse = DirectedSymbol Response Example.darkResponse
                recorderRef <- newSTRef recorderCleanState
                offerDirectedSymbolST recorderRef darkResponse
                offerDirectedSymbolST recorderRef darkResponse
                readSTRef recorderRef
           )
         putStrLn ""
         putStrLn $ "Model of observed behavior with one or two updated flags set"
         logVerbose "Recorder dirty state" [logs recorderDirtyState]
         return ()
    otherwise -> return ()

  putStrLn ""

  putStrLn $ "The end"
  return ()
