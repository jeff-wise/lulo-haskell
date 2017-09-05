
{-| Built-In Constraints
-}


{-# LANGUAGE OverloadedStrings #-}


module Lulo.Schema.Constraints where


import Lulo.Schema.Types

import qualified Data.Text as T



builtInConstraint :: ConstraintName -> Maybe Constraint
builtInConstraint (ConstraintName _constraintName) =
  case _constraintName of
    "positive_integer" -> Just positiveIntegerConstraint
    _                  -> Nothing


positiveIntegerConstraint :: Constraint
positiveIntegerConstraint = 
  Constraint
    (ConstraintData 
      (ConstraintName $ T.pack "Positive Integer") 
      (Just $ ConstraintDescription $ T.pack "Any integer i > 0"))
    (NumGreaterThan $ NumberGreaterThanConstraint 0)



