

{-| Parse
-}


module Lulo.Parse where


import Lulo.Types
import Lulo.Types.Spec.JSON

import Data.Yaml as YAML (ParseException (..))
import qualified Data.Yaml as YAML (decodeFileEither)



parseSpecFile :: FilePath -> IO (Maybe Spec) 
parseSpecFile filepath = do
  eSpec <- YAML.decodeFileEither filepath
  case eSpec of
    Left  exception -> do
      handleParseException exception
      return Nothing
    Right spec      -> return $ Just spec


handleParseException :: YAML.ParseException -> IO ()
handleParseException exception = putStrLn $ show exception
  -- case exception of
  --   InvalidYaml (Just exception) -> putStrLn $ show exception
  --   _                            -> return ()
