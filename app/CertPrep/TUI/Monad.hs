module CertPrep.TUI.Monad where

import Brick (EventM, halt)
import Brick.BChan (BChan, writeBChan)
import CertPrep.Exam (
    ActivePhase,
    AnsweringData,
    AppState,
    ExamPhase (..),
    Name,
    ReviewingData,
    examPhase,
 )
import CertPrep.Explanations (
    ExplainEnv (explainStream),
    ExplainEvent,
    ExplainRequest (..),
    MonadExplain (..),
 )
import Control.Concurrent (forkIO)
import Lens.Micro.Mtl (use)

data CustomEvent = Tick | ExplanationEvent Int ExplainEvent

data TuiEnv = TuiEnv
    { tuiConfigPath :: FilePath
    , tuiEventChan :: BChan CustomEvent
    , tuiExplainEnv :: Maybe ExplainEnv -- Nothing = AI disabled
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
        _ -> return ()
whenReviewing :: (ActivePhase ReviewingData -> TuiM ()) -> TuiM ()
whenReviewing f = do
    phase <- use examPhase
    case phase of
        Reviewing rp -> f rp
        _ -> return ()

instance MonadExplain TuiM where
    explainAvailable = asks (isJust . tuiExplainEnv)
    requestExplanation req = do
        env <- ask
        for_ (tuiExplainEnv env) $ \explainEnv ->
            liftIO . void . forkIO $
                explainStream explainEnv (reqPrompt req) $ -- total, never throws
                    writeBChan (tuiEventChan env) . ExplanationEvent (reqId req)
