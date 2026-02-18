module CLI (CLIOptions (..), parseCLIOpts) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read (decimal)
import Options.Applicative

data CLIOptions = CLIOptions
    { cliSampleAmount :: Maybe Int
    , cliWeights :: [(Text, Int)]
    , cliConfigPath :: Maybe FilePath
    }

cliParser :: Parser CLIOptions
cliParser =
    CLIOptions
        <$> optional sampleAmount
        <*> many weights
        <*> optional (argument str (metavar "<config.json>"))
  where
    sampleAmount =
        option
            auto
            ( short 'n'
                <> long "sample-amount"
                <> metavar "N"
                <> help "Number of questions to sample"
            )
    weights =
        option
            parseWeight
            ( short 'w'
                <> long "weight"
                <> metavar "CATEGORY:WEIGHT"
                <> help
                    "Category weight (repeatable), e.g. \"AWS Storage:2\""
            )

parseWeight :: ReadM (Text, Int)
parseWeight = eitherReader $ \s ->
    case T.splitOn ":" (T.pack s) of
        [cat, wText] | Right (w, "") <- decimal wText -> Right (cat, w)
        _ -> Left $ "Invalid weight format: " ++ s ++ " (expected CATEGORY:WEIGHT)"

cliInfo :: ParserInfo CLIOptions
cliInfo =
    info
        (cliParser <**> helper)
        ( fullDesc
            <> progDesc "Certification exam prep TUI"
            <> header "cert-prep - practice certification questions"
        )

parseCLIOpts :: IO CLIOptions
parseCLIOpts = execParser cliInfo
