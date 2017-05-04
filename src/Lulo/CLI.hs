
{-| Lulo Command Line Interface
-}


module Lulo.CLI where


import Lulo.HTML (specHTML)
import Lulo.Parse (parseSpecFile)
import Lulo.Types (
    Parameters (..)
  , specFilename, verbosity, htmlFilename, htmlFilePretty, cssFilename
  , Verbosity (..)
  , Spec
  )

import Control.Lens
import Control.Monad (when)

import qualified Data.ByteString.Lazy as BL
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

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
              <*> flag Normal Verbose
                  (  long "verbose" 
                  <> short 'v' 
                  <> help "Enable verbose mode." )
              <*> (optional $ strOption
                    (  long "html"
                    <> metavar "HTML"
                    <> help "The file path of the generated HTML file." ) )
              <*> (optional $ strOption
                    (  long "css"
                    <> metavar "CSS"
                    <> help "The file path of a CSS file for the HTML file." ) )
              <*> switch
                  (  long "html-pretty" 
                  <> help "Output pretty printed HTML." )


-- PARSE FILE
--------------------------------------------------------------------------------

parseSpec :: Parameters -> IO ()
parseSpec parameters = do
  let isVerbose = (parameters ^. verbosity) == Verbose
  mSpec <- parseSpecFile (parameters ^. specFilename)
  case mSpec of
    Just spec -> processSpec spec parameters
    Nothing   -> 
      when isVerbose $ 
        putStrLn $ "Could not be parse " <> (parameters ^. specFilename)


processSpec :: Spec -> Parameters -> IO ()
processSpec spec parameters = do
  let isVerbose = (parameters ^. verbosity) == Verbose
  -- Show message
  when isVerbose $
    putStrLn "Spec parsed successfully."
  -- HTML generation
  case (parameters ^. htmlFilename) of
    Just filename -> 
      generateHTMLFile spec filename parameters
    Nothing           -> 
      when isVerbose $
        putStrLn "No HTML file name provided. None will be generated."


generateHTMLFile :: Spec -> FilePath -> Parameters -> IO () 
generateHTMLFile spec filename parameters = do
  let mCSSFilePath = (parameters ^. cssFilename) 
  case (parameters ^. htmlFilePretty) of
    True -> writeFile filename $ 
              Pretty.renderHtml $ specHTML spec mCSSFilePath
    False -> BL.writeFile filename $ renderHtml $ specHTML spec mCSSFilePath

