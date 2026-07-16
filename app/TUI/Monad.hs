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
    ExamPhase (..),
    Name,
    ReviewingData,
    examPhase,
 )
import Explanations (
    ExplainEnv (explainFetch),
    ExplainError,
    ExplainRequest (..),
    MonadExplain (..),
 )
import Lens.Micro.Mtl (use)

data CustomEvent = Tick | ExplanationReceived Int (Either ExplainError Text)

data TuiEnv = TuiEnv
    { tuiConfigPath :: FilePath
    , tuiEventChan :: BChan CustomEvent
    , tuiExplainEnv :: Maybe ExplainEnv -- Nothing = AI disabled
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

instance MonadExplain TuiM where
    explainAvailable = asks (isJust . tuiExplainEnv)
    requestExplanation req = do
        env <- ask
        for_ (tuiExplainEnv env) $ \explainEnv -> liftIO . void . forkIO $ do
            result <- explainFetch explainEnv (reqPrompt req) -- total, never throws
            writeBChan (tuiEventChan env) (ExplanationReceived (reqQuestionIndex req) result)
