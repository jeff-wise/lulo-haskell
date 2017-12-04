
{-| Lulo Command Line Interface
-}


module Lulo.CLI where


import Lulo.Document (parseDocumentFile)
import Lulo.HTML as LuloHtml
import Lulo.HTML.Types (HtmlSettings (..), defaultHtmlSettings)
import Lulo.Types
import Lulo.Schema (parseSchemaFile)
import Lulo.Schema.SchemaSchema (schemaSchema)
import Lulo.Schema.Types (Schema)
import Lulo.Schema.Index (schemaIndex)

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Yaml as Yaml (decodeFileEither)

import qualified Text.Blaze.Html.Renderer.Pretty as Pretty (renderHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Options.Applicative



-- | Top-level command line parser. Adds simple help text to the parameter
-- parser
cli :: IO ()
cli = execParser opts >>= run
  where
  opts =
    info (helper <*> commandParser) (
         fullDesc
      <> progDesc "Parse and display Lulo specifications."
      <> header "Lulo" )


-- COMMANDS
--------------------------------------------------------------------------------

data Command =
    CmdCheck CheckParameters
  | CmdHtml HtmlParameters
  | CmdSchemaSchema SchemaSchemaParameters
  deriving (Eq, Show)


-- Commands > Parser
--------------------------------------------------------------------------------

-- | Parse the subcommands 
commandParser :: Parser Command
commandParser = hsubparser ( 
     command "check" (info checkParser checkInfo)
  <> command "html" (info htmlParser htmlInfo)
  <> command "schemaschema" (info ssParser ssInfo) )
  where
    checkInfo =
         fullDesc
      <> progDesc "Validate a document against a schema."
      <> header   "Validate a document."
    htmlInfo =
         fullDesc
      <> progDesc "Generate HTML documenation for a schema."
      <> header    "HTML Generation"
    ssInfo =
         fullDesc
      <> progDesc "Schema Schema commands."
      <> header    "Schema Schema"


-- Commands > Parser > Check
--------------------------------------------------------------------------------

-- | Parser the parameters from the command line
checkParser :: Parser Command
checkParser = CmdCheck <$> params
  where
    params = CheckParameters
      <$> argument str (metavar "DOCUMENT")
      <*> argument str (metavar "SCHEMA")


-- Commands > Parser > Html
--------------------------------------------------------------------------------

-- | Parser the parameters from the command line
htmlParser :: Parser Command
htmlParser = CmdHtml <$> params
  where
    params = HtmlParameters
      <$> strOption
          (  long "schema-file"
          <> short 's'
          <> help "The file path of the Lulo schema." )
      <*> flag Normal Verbose
          (  long "verbose" 
          <> short 'v' 
          <> help "Enable verbose mode." )
      <*> strOption
          (  long "html-file" 
          <> short 'h' 
          <> help "The name of the generated HTML file." )
      <*> optional (strOption
          (  long "html-options"
          <> help "The file path of the HTML options file." ) )


-- Commands > Parser > SchemaSchema 
--------------------------------------------------------------------------------

-- | Parse the options for the Service subcommand
ssParser :: Parser Command
ssParser = CmdSchemaSchema <$> params
  where
    params = SchemaSchemaParameters 
      <$> argument auto (metavar "COMMAND")
      <*> optional (strOption
          (  long "html-options"
          <> help "The file path of the HTML options file." ) )
      <*> optional (strOption
          (  long "html-file"
          <> short 'f'
          <> help "The file path of the generated HTML file." ) )


-- RUN
--------------------------------------------------------------------------------

run :: Command -> IO ()
run cmd = do
  eResult <- runExceptT $ getCLI runCommand
  case eResult of 
    Right _ -> return ()
    Left  err -> putStrLn $ "Error Occurred:\n\n" <> show err
  where
    runCommand :: CLI ()
    runCommand = case cmd of
      CmdCheck        params -> checkCommand params
      CmdHtml         params -> generateHtmlCommand params
      CmdSchemaSchema params -> schemaSchemaCommand params


-- Run > Check
--------------------------------------------------------------------------------

checkCommand :: CheckParameters -> CLI ()
checkCommand params = liftIO $ do
  eSchema <- parseSchemaFile $ checkParamsSchemaFilePath params
  case eSchema of
    Left err -> putStrLn $ "Could not parse schema:\n\n" <> show err
    Right schema -> do
      eDoc <- parseDocumentFile (checkParamsDocumentFilePath params) schema
      case eDoc of
        Left err -> putStrLn $ "Could not parse document:\n\n" <> show err
        Right _  -> putStrLn "Document is member of schema."


-- Run > Generate Html
--------------------------------------------------------------------------------

generateHtmlCommand :: HtmlParameters -> CLI ()
generateHtmlCommand params = do
  -- let isVerbose = htmlParamsVerbosity params == Verbose
  schema <- parseSchema $ htmlParamsSchemaFilename params
  opts <- parseHtmlOptions $ htmlParamsOptionsFilePath params
  let genHtmlFilePath = htmlParamsHtmlFilePath params
  writeHtmlFile schema genHtmlFilePath opts
  

-- Run > Schema Schema
--------------------------------------------------------------------------------

schemaSchemaCommand :: SchemaSchemaParameters -> CLI ()
schemaSchemaCommand params =
  case ssParamsCommand params of
    SchemaSchemaHtml -> generateSchemaHtmlFile

  where

    generateSchemaHtmlFile :: CLI ()
    generateSchemaHtmlFile = do
      opts <- parseHtmlOptions $ ssParamsHtmlOptionsFilePath params
      let genHtmlFilePath = fromMaybe "schema.html" $ ssParamsHtmlFilePath params
      writeHtmlFile schemaSchema genHtmlFilePath opts


-- Run > General Combinators
--------------------------------------------------------------------------------

parseSchema :: FilePath -> CLI Schema
parseSchema schemaFilePath = do
  eSchema <- liftIO $ parseSchemaFile schemaFilePath
  case eSchema of
    Right schema -> do
      liftIO $ putStrLn "Schema parsed successfully."
      return schema
    Left  err    -> throwError $ CouldNotParseSchema err


parseHtmlOptions :: Maybe FilePath -> CLI HtmlSettings
parseHtmlOptions mOptionsFilePath =
  case mOptionsFilePath of
    Just filepath -> liftIO $ do
      eSettings <- Yaml.decodeFileEither filepath
      case eSettings of
        Right settings -> return settings  
        Left  _        -> return defaultHtmlSettings
    Nothing       -> return defaultHtmlSettings


writeHtmlFile :: Schema -> FilePath -> HtmlSettings -> CLI ()
writeHtmlFile schema filename htmlSettings = liftIO $
  if htmlSettingsPrintPretty htmlSettings
     then writeFile filename $ 
       Pretty.renderHtml $ LuloHtml.schemaDoc (schemaIndex schema) htmlSettings
     else BL.writeFile filename $ renderHtml $ 
       LuloHtml.schemaDoc (schemaIndex schema) htmlSettings


