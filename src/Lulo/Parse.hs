

{-| Parse
-}


module Lulo.Parse where


import Lulo.Spec.Types (Spec)
import Lulo.Spec.JSON ()

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
handleParseException = print
  -- case exception of
  --   InvalidYaml (Just exception) -> putStrLn $ show exception
  --   _                            -> return ()
