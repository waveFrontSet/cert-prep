module TUI.Monad where

import Brick (EventM, halt)
import Brick.BChan (BChan, writeBChan)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.Reader (MonadIO (..), MonadReader (ask), ReaderT (..))
import Control.Monad.Reader.Class (asks)
import Control.Monad.State (MonadState, MonadTrans (..))
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.Text (Text)
import Exam (
    ActivePhase,
    AnsweringData,
    AppState,
    ExamCore,
    ExamPhase (..),
    ExplainingData,
    Name,
    ReviewingData,
    examPhase,
 )
import Explanations (
    ExplainConfig,
    ExplainError,
    ExplainRequest (..),
    MonadExplain (..),
    fetchExplanation,
 )
import Lens.Micro.Mtl (use, (.=))

data CustomEvent = Tick | ExplanationReceived Int (Either ExplainError Text)

data TuiEnv = TuiEnv
    { tuiConfigPath :: FilePath
    , tuiEventChan :: BChan CustomEvent
    , tuiExplainCfg :: Maybe ExplainConfig -- Nothing = AI disabled
    }

newtype TuiM a = TuiM {unTuiM :: ReaderT TuiEnv (EventM Name AppState) a}
    deriving (Functor, Applicative, Monad, MonadReader TuiEnv, MonadState AppState, MonadIO)

runTuiM :: TuiEnv -> TuiM a -> EventM Name AppState a
runTuiM env t = runReaderT (unTuiM t) env

tuiHalt :: TuiM ()
tuiHalt = TuiM (lift halt)

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
whenExplaining :: (ActivePhase ExplainingData -> TuiM ()) -> TuiM ()
whenExplaining f = do
    phase <- use examPhase
    case phase of
        Explaining ep -> f ep
        _ -> return ()
whenChecking :: (ExamCore -> TuiM ()) -> TuiM ()
whenChecking f = do
    phase <- use examPhase
    case phase of
        CheckingTrophies core -> f core
        _ -> return ()
modifyPhase :: (ExamPhase -> ExamPhase) -> TuiM ()
modifyPhase f = do
    phase <- use examPhase
    examPhase .= f phase

instance MonadExplain TuiM where
    explainAvailable = asks (isJust . tuiExplainCfg)
    requestExplanation req = do
        env <- ask
        for_ (tuiExplainCfg env) $ \cfg -> liftIO . void . forkIO $ do
            result <- fetchExplanation cfg (reqPrompt req) -- total, never throws
            writeBChan (tuiEventChan env) (ExplanationReceived (reqQuestionIndex req) result)
