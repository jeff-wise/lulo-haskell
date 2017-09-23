
{-| Lulo Command Line Interface
-}


module Lulo.CLI where


import Lulo.HTML as LuloHtml
import Lulo.HTML.Types (HtmlSettings (..), defaultHtmlSettings)
import Lulo.Types
import Lulo.Schema
import Lulo.Schema.Types (Schema)
import Lulo.Schema.Index (schemaIndex)

import Control.Error.Util (hush)
import Control.Monad (when)

import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Yaml as Yaml (decodeFileEither)

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
                  (  long "html-file" 
                  <> short 'h' 
                  <> help "Generate an HTML version of the schema." ) )
              <*> optional (strOption
                  (  long "html-options"
                  <> help "The file path of the HTML options yaml file." ) )
              -- <*> optional (strOption
              --     (  long "html-css"
              --     <> metavar "CSS"
              --     <> help "The file path of a CSS file for the HTML file." ) )
              -- <*> switch
              --     (  long "html-pretty" 
              --     <> help "Output pretty printed HTML." )


-- PARSE FILE
--------------------------------------------------------------------------------

run :: Parameters -> IO ()
run parameters = do
  -- Parse Schema File
  eSchema <- parseSchemaFile $ parametersSchemaFilename parameters
  case eSchema of
    Right schema -> processSchema schema parameters
    Left  err    -> print err


processSchema :: Schema -> Parameters -> IO ()
processSchema schema parameters = do
  let isVerbose = parametersVerbosity parameters == Verbose
  -- Show message
  when isVerbose $ putStrLn "Schema parsed successfully."
  -- HTML generation
  case parametersHtmlFilePath parameters of
    Just filepath -> do
      -- Try to parse HTML Options
      let mOptionsFp = parametersHtmlOptionsFilePath parameters
      htmlSettings <- case mOptionsFp of
                        Just optionsFp -> fromMaybe defaultHtmlSettings . hush 
                                            <$> Yaml.decodeFileEither optionsFp
                        Nothing        -> return defaultHtmlSettings
      generateHTMLFile schema filepath htmlSettings
    Nothing       ->
      when isVerbose $
        putStrLn "No HTML file name provided. None will be generated."


generateHTMLFile :: Schema -> FilePath -> HtmlSettings -> IO () 
generateHTMLFile schema filename htmlSettings =
  if htmlSettingsPrintPretty htmlSettings
     then writeFile filename $ 
       Pretty.renderHtml $ LuloHtml.schemaDoc (schemaIndex schema) htmlSettings
     else BL.writeFile filename $ renderHtml $ 
       LuloHtml.schemaDoc (schemaIndex schema) htmlSettings

