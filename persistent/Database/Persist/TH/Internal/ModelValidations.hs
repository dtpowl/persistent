{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.TH.Internal.ModelValidations where

import Language.Haskell.TH.Syntax
import System.Environment.Blank (getEnv)
import Control.Monad.Logger (logWarnN)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map, (!))
import qualified Data.Map as M

data ModelValidation = ModelValidation { validationName :: Text
                                       , validationPredicate :: Text -> Bool
                                       , validationMessage :: String
                                       }

data ValidationLevel = Error | Warning | Disabled

allValidations :: [ModelValidation]
allValidations = [
  ModelValidation { validationName == "NoTabs"
                  , validationPredicate = (T.elem '\t')
                  , validationMessage = "Tabs disallowed in model files"
                  }
  ]

validationsByName :: Map Text ModelValidation
validationsByName = M.fromList $ map (\mv -> (validationName mv, mv)) allValidations

activeValidationsForLevel :: ValidationLevel -> Q [ModelValidation]
activeValidationsForLevel Disabled = mempty
activeValidationsForLevel level = do
  let varName = case level of
                  Error -> "PERSISTENT_MODEL_FILE_VALIDATION_ERRORS"
                  Warning -> "PERSISTENT_MODEL_FILE_VALIDATION_WARNINGS"
  setting <- (qRunIO . getEnv) varName
  let maybeNames = T.splitOn "," <$> T.pack <$> setting
  pure $ maybe [] (map $ (!) validationsByName) maybeNames

activeValidations :: Q [(ModelValidation, ValidationLevel)]
activeValidations = do
  errorValidations <- activeValidationsForLevel Error
  warningValidations <- activeValidationsForLevel Warning
  pure $ map (,Error) errorValidations <> map (,Warning) warningValidations

renderValidationMessage :: ModelValidation -> String -> String
renderValidationMessage v fp = mconcat [ "in model file "
                                       , fp
                                       , ": "
                                       , validationMessage v
                                       , " ("
                                       , validationName v
                                       , ")"
                                       ]

runValidation :: String -> Text -> ModelValidation -> ValidationLevel -> Q Bool
runValidation filePath text v level = do
  let failed = (validationPredicate v) text
  let message = renderValidationMessage v filePath
  case (level, failed) of
    (Disabled, _)   -> pure True
    (_, False)      -> pure True
    (Warning, True) -> (reportWarning message) >> pure False
    (Error, True)   -> error message

validateModelFileContents :: String -> Text -> Q Bool
validateModelFileContents fp t = do
  validations <- activeValidations
  results <- sequence $ map (uncurry $ runValidation fp t) validations
  pure . and $ results
