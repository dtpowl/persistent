module Database.Persist.Quasi.PersistSettings where

import Data.Char (isDigit, isLower, isSpace, isUpper, toLower)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty (..))
import Data.Void (Void)
import Database.Persist.Types
import Text.Megaparsec ( ParseError
                       , SourcePos
                       , PosState
                       , pstateSourcePos
                       , ParseErrorBundle (..)
                       , errorBundlePretty
                       )

data PersistSettings = PersistSettings
    { psToDBName :: !(Text -> Text)
    -- ^ Modify the Haskell-style name into a database-style name.
    , psToFKName :: !(EntityNameHS -> ConstraintNameHS -> Text)
    -- ^ A function for generating the constraint name, with access to
    -- the entity and constraint names. Default value: @mappend@
    --
    -- @since 2.13.0.0
    , psStrictFields :: !Bool
    -- ^ Whether fields are by default strict. Default value: @True@.
    --
    -- @since 1.2
    , psIdName :: !Text
    -- ^ The name of the id column. Default value: @id@
    -- The name of the id column can also be changed on a per-model basis
    -- <https://github.com/yesodweb/persistent/wiki/Persistent-entity-syntax>
    --
    -- @since 2.0
    , psTabErrorLevel :: Maybe ParserErrorLevel
    -- ^ Whether and with what severity to disallow tabs in entity source text.
    --
    -- @since 2.16.0.0
    }

defaultPersistSettings, upperCaseSettings, lowerCaseSettings :: PersistSettings
defaultPersistSettings =
    PersistSettings
        { psToDBName = id
        , psToFKName = \(EntityNameHS entName) (ConstraintNameHS conName) -> entName <> conName
        , psStrictFields = True
        , psIdName = "id"
        , psTabErrorLevel = Just LevelWarning
        }
upperCaseSettings = defaultPersistSettings
lowerCaseSettings =
    defaultPersistSettings
        { psToDBName =
            let
                go c
                    | isUpper c = T.pack ['_', toLower c]
                    | otherwise = T.singleton c
             in
                T.dropWhile (== '_') . T.concatMap go
        }

-- @since 2.16.0.0
data ParserErrorLevel = LevelError | LevelWarning deriving (Eq, Show)

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
