
{-| Lulo Command Line Interface
-}


module Lulo.CLI where


import Lulo.Parse (parseSpecFile)
import Lulo.Types (Parameters (..))

import Control.Monad (when)

import Data.Monoid ((<>))

import Options.Applicative



-- RUN
--------------------------------------------------------------------------------

-- | Read and parse the command line arguments
run :: IO ()
run = execParser cliParser >>= parseSpec


-- COMMAND LINE INTERFACE
--------------------------------------------------------------------------------

-- > Types
--------------------------------------------------------------------------------

-- > Parser
--------------------------------------------------------------------------------

-- | Top-level command line parser. Adds simple help text to the parameter
-- parser
cliParser :: ParserInfo Parameters
cliParser = info (parameterParser <**> helper)
             (  fullDesc
             <> progDesc "Parse and display Lulo specifications."
             <> header "LULO" )


-- | Parser the parameters from the command line
parameterParser :: Parser Parameters
parameterParser = Parameters
              <$> strOption
                  (  long "file"
                  <> short 'f'
                  <> metavar "FILE"
                  <> help "The file path of the lulo specification." )
              <*> switch
                  (  long "verbose" 
                  <> short 'v' 
                  <> help "Indicates that debug messages should be printed." )


-- PARSE FILE
--------------------------------------------------------------------------------

parseSpec :: Parameters -> IO ()
parseSpec (Parameters filename showDebug) = do
  mSpec <- parseSpecFile filename
  case mSpec of
    Just spec -> do
      when showDebug $
        putStrLn "Spec parsed successfully."
    Nothing   -> 
      when showDebug $ 
        putStrLn $ "Could not be parse " <> filename
