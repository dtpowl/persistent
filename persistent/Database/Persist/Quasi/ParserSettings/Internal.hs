module Database.Persist.Quasi.ParserSettings.Internal where

import Data.Void (Void)
import Text.Megaparsec ( ParseError
                       , SourcePos
                       , PosState
                       , pstateSourcePos
                       , ParseErrorBundle (..)
                       , errorBundlePretty
                       )
import Data.List.NonEmpty (NonEmpty (..))

-- @since 2.16.0.0
data ParserErrorLevel = LevelError | LevelWarning deriving (Eq, Show)

-- @since 2.16.0.0
newtype ParserSettings = ParserSettings {parserTabErrorLevel :: Maybe ParserErrorLevel}

-- @since 2.16.0.0
defaultParserSettings :: ParserSettings
defaultParserSettings = ParserSettings{parserTabErrorLevel = Just LevelWarning}

-- @since 2.16.0.0
data ParserWarning = ParserWarning
    { parserWarningExtraMessage :: String
    , parserWarningUnderlyingError :: ParseError String Void
    , parserWarningPosState :: PosState String
    }
    deriving (Eq, Show)

warningPos :: ParserWarning -> SourcePos
warningPos = pstateSourcePos . parserWarningPosState

instance Ord ParserWarning where
    l <= r =
      if warningPos l == warningPos r
      then parserWarningMessage l <= parserWarningMessage r
      else warningPos l <= warningPos r

-- @since 2.16.0.0
parserWarningMessage :: ParserWarning -> String
parserWarningMessage pw =
    parserWarningExtraMessage pw
        <> ( errorBundlePretty $
                ParseErrorBundle
                    { bundleErrors = parserWarningUnderlyingError pw :| []
                    , bundlePosState = parserWarningPosState pw
                    }
           )
