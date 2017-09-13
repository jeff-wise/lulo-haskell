
{-| Lulo Command Line Interface
-}


module Lulo.CLI where


import Lulo.HTML as LuloHtml
import Lulo.HTML.Types (HtmlSettings (..))
import Lulo.Types
import Lulo.Schema
import Lulo.Schema.Types (Schema)
import Lulo.Schema.Index (schemaIndex)

import Control.Lens
import Control.Monad (when)

import qualified Data.ByteString.Lazy as BL
import Data.Monoid ((<>))

import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Options.Applicative



-- COMMAND LINE INTERFACE
--------------------------------------------------------------------------------

-- | Read and parse the command line arguments
cli :: IO ()
cli = execParser cliParser >>= run


-- CLI > Parser
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
              <*> optional (strOption
                  (  long "html-out"
                  <> metavar "HTML"
                  <> help "The file path of the generated HTML file." ) )
              <*> optional (strOption
                  (  long "html-css"
                  <> metavar "CSS"
                  <> help "The file path of a CSS file for the HTML file." ) )
              <*> switch
                  (  long "html-pretty" 
                  <> help "Output pretty printed HTML." )


-- PARSE FILE
--------------------------------------------------------------------------------

run :: Parameters -> IO ()
run parameters = do
  eSchema <- parseSchemaFile $ parameters ^. specFilename
  case eSchema of
    Right schema -> processSchema schema parameters
    Left  err    -> print err


processSchema :: Schema -> Parameters -> IO ()
processSchema schema parameters = do
  let isVerbose = (parameters ^. verbosity) == Verbose
  -- Show message
  when isVerbose $
    putStrLn "Schema parsed successfully."
  -- HTML generation
  case parameters ^. htmlFilename of
    Just filename -> 
      generateHTMLFile schema filename parameters
    Nothing           -> 
      when isVerbose $
        putStrLn "No HTML file name provided. None will be generated."


generateHTMLFile :: Schema -> FilePath -> Parameters -> IO () 
generateHTMLFile spec filename parameters = do
  let mCSSFilePath = parameters ^. cssFilename
      _specIndex   = schemaIndex spec
  if parameters ^. htmlFilePretty
     then writeFile filename $ 
       Pretty.renderHtml $ LuloHtml.specDoc _specIndex 
                                            (HtmlSettings mCSSFilePath)
     else BL.writeFile filename $ renderHtml $ 
              LuloHtml.specDoc _specIndex (HtmlSettings mCSSFilePath)

