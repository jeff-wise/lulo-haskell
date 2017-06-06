
{-| Built-In Constraints
-}


module Lulo.Spec.Constraints where


import Lulo.Spec.Types



builtInConstraint :: ConstraintName -> Maybe Constraint
builtInConstraint (ConstraintName constraintName) =
  case constraintName of
    "positive_integer" -> Just positiveIntegerConstraint
    _                  -> Nothing


positiveIntegerConstraint :: Constraint
positiveIntegerConstraint = 
  Constraint
    (ConstraintData 
      (ConstraintName $ T.pack "Positive Integer") 
      (Just $ ConstraintDescription $ T.pack "Any integer i > 0"))
    (NumGreaterThan $ NumberGreaterThanConstraint 0)



