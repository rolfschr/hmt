module Options where

import Options.Applicative

data Options = Options
    { width :: Int}

opts :: ParserInfo Options
opts = info (opts' <**> helper)
    ( fullDesc
    <> progDesc "Format text."
    <> header "hmt - A Simple Text Formatter")

opts' :: Parser Options
opts' = Options <$> width'


width' :: Parser Int
width' = option auto
    ( long "width"
   <> short 'w'
   <> metavar "N"
   <> value 79
   <> showDefault
   <> help "Maximum line width" )
