module Options where

import Options.Applicative

data Options = Options
    { exec :: String,
      args :: [String]}

opts :: ParserInfo Options
opts = info (opts' <**> helper)
    ( fullDesc
    <> progDesc "Format text."
    <> header "hmt - A Simple Wrapper Around fmt")

opts' :: Parser Options
opts' = Options <$> exec' <*> args'

exec' :: Parser String
exec' = strOption
    ( long "exec"
   <> short 'e'
   <> metavar "EXECUTABLE"
   <> value "fmt"
   <> showDefault
   <> help "The name or path to fmt" )

args' :: Parser [String]
args' = many (argument str
    ( metavar "ARGUMENTS ..."))
