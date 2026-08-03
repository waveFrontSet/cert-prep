module CertPrep.TUI.Monad where

import Brick (EventM, halt)
import Brick.BChan (BChan, writeBChan)
import Control.Concurrent (forkIO)
import Lens.Micro.Mtl (use)

import CertPrep.Exam (
  ActivePhase,
  AnsweringData,
  AppState,
  CustomEvent (..),
  ExamCore,
  ExamPhase (..),
  ExplainingData,
  ExportingData,
  FinishedState,
  Name,
  ReviewingData,
  TrophyAwardedData,
  examPhase,
 )
import CertPrep.Explanations (
  ExplainEnv (explainStream),
  ExplainRequest (..),
  MonadExplain (..),
 )

data TuiEnv = TuiEnv {
  tuiConfigPath :: FilePath,
  tuiEventChan :: BChan CustomEvent,
  tuiExplainEnv :: Maybe ExplainEnv -- Nothing = AI disabled
}

newtype TuiM a = TuiM {unTuiM :: ReaderT TuiEnv (EventM Name AppState) a}
  deriving (Functor, Applicative, Monad, MonadReader TuiEnv, MonadState AppState, MonadIO)

runTuiM :: TuiEnv -> TuiM a -> EventM Name AppState a
runTuiM env t = runReaderT (unTuiM t) env

-- Lift a raw brick EventM action (halt, viewport scrolling, …) into TuiM.
liftEvent :: EventM Name AppState a -> TuiM a
liftEvent = TuiM . lift

tuiHalt :: TuiM ()
tuiHalt = liftEvent halt

whenAnswering :: (ActivePhase AnsweringData -> TuiM ()) -> TuiM ()
whenAnswering f = do
  phase <- use examPhase
  case phase of
    Answering ap -> f ap
    _ -> pass
whenReviewing :: (ActivePhase ReviewingData -> TuiM ()) -> TuiM ()
whenReviewing f = do
  phase <- use examPhase
  case phase of
    Reviewing rp -> f rp
    _ -> pass
whenExplaining :: (ActivePhase ExplainingData -> TuiM ()) -> TuiM ()
whenExplaining f = do
  phase <- use examPhase
  case phase of
    Explaining ep -> f ep
    _ -> pass
whenCheckingTrophies :: (ExamCore -> TuiM ()) -> TuiM ()
whenCheckingTrophies f = do
  phase <- use examPhase
  case phase of
    CheckingTrophies core -> f core
    _ -> pass
whenTrophyAwarded :: (TrophyAwardedData -> TuiM ()) -> TuiM ()
whenTrophyAwarded f = do
  phase <- use examPhase
  case phase of
    TrophyAwarded tad -> f tad
    _ -> pass
whenFinished :: (FinishedState -> TuiM ()) -> TuiM ()
whenFinished f = do
  phase <- use examPhase
  case phase of
    Finished fs -> f fs
    _ -> pass
whenExporting :: (ExportingData -> TuiM ()) -> TuiM ()
whenExporting f = do
  phase <- use examPhase
  case phase of
    Exporting ed -> f ed
    _ -> pass

instance MonadExplain TuiM where
  explainAvailable = asks (isJust . tuiExplainEnv)
  requestExplanation req = do
    env <- ask
    for_ (tuiExplainEnv env) $ \explainEnv ->
      liftIO . void . forkIO $
        explainStream explainEnv (reqPrompt req) $ -- total, never throws -- total, never throws -- total, never throws -- total, never throws
        -- total, never throws
          writeBChan (tuiEventChan env) . ExplanationEvent (reqId req)
