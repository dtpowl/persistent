{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Database.Persist.Quasi.Internal.ModelParser
    ( SourceLoc (..)
    , Token (..)
    , tokenContent
    , anyToken
    , ParsedEntityDef
    , parsedEntityDefComments
    , parsedEntityDefEntityName
    , parsedEntityDefIsSum
    , parsedEntityDefEntityAttributes
    , parsedEntityDefFieldAttributes
    , parsedEntityDefExtras
    , parsedEntityDefSpan
    , parseSource
    , memberBlockAttrs
    ) where

import Control.Monad (mzero)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import Database.Persist.Types
import Database.Persist.Types.SourceSpan
import Language.Haskell.TH.Syntax (Lift)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- | Source location: file and line/col information. This is half of a 'SourceSpan'.
--
-- @since 2.16.0.0
data SourceLoc = SourceLoc
    { locFile :: Text
    , locStartLine :: Int
    , locStartCol :: Int
    }
    deriving (Show, Lift)

-- @since 2.16.0.0
data Token
    = DocComment Text
    | Comment Text
    | Quotation Text
    | Equality Text Text
    | Parenthetical Text
    | BlockKey Text
    | PText Text
    deriving (Eq, Ord, Show)

-- | Converts a token into a Text representation for second-stage parsing or presentation to the user
--
-- @since 2.16.0.0
tokenContent :: Token -> Text
tokenContent = \case
    Comment s -> s
    DocComment s -> s
    Quotation s -> s
    Equality l r -> mconcat [l, "=", r]
    Parenthetical s -> s
    PText s -> s
    BlockKey s -> s

spaceConsumer :: Parser ()
spaceConsumer =
    L.space
        hspace1
        (L.skipLineComment "--")
        empty

indentationConsumer :: Parser ()
indentationConsumer =
    L.space
        space1
        empty
        empty

contentChar :: Parser Char
contentChar =
    choice
        [ alphaNumChar
        , char '.'
        , char '['
        , char ']'
        , char '_'
        , char '\''
        , char '"'
        , char '!'
        , char '~'
        , char '-'
        , char ':'
        , do
            backslash <- char '\\'
            nextChar <- lookAhead anySingle
            if nextChar == '(' || nextChar == ')'
                then single nextChar
                else pure backslash
        ]

nonLineSpaceChar :: Parser Char
nonLineSpaceChar = choice [char ' ', char '\t']

-- This is a replacement for `Text.Megaparsec.Char.Lexer.charLiteral`;
-- it does nearly the same thing but additionally supports escaped parentheses.

charLiteral :: Parser Char
charLiteral = label "literal character" $ do
    char1 <- anySingle
    case char1 of
        '\\' -> do
            char2 <- anySingle
            case char2 of
                '(' -> pure '('
                ')' -> pure ')'
                '\\' -> pure '\\'
                '\"' -> pure '\"'
                '\'' -> pure '\''
                _ -> unexpected (Tokens $ char2 :| [])
        _ -> pure char1

equality :: Parser Token
equality = label "equality expression" $ do
    L.lexeme spaceConsumer $ do
        lhs <- some contentChar
        _ <- char '='
        rhs <-
            choice
                [ quotation'
                , sqlLiteral
                , parentheticalInner
                , some $ contentChar <|> char '(' <|> char ')'
                ]
        pure $ Equality (Text.pack lhs) (Text.pack rhs)
  where
    parentheticalInner = do
        str <- parenthetical'
        pure . init . drop 1 $ str

sqlTypeName :: Parser String
sqlTypeName =
    some $
        choice
            [ alphaNumChar
            , char '_'
            ]

sqlLiteral :: Parser String
sqlLiteral = label "SQL literal" $ do
    quote <- L.lexeme spaceConsumer $ char '\'' *> manyTill charLiteral (char '\'')
    st <- optional $ do
        colons <- string "::"
        tn <- sqlTypeName
        pure $ colons <> tn
    pure $
        mconcat
            [ "'"
            , quote
            , "'"
            , fromMaybe "" st
            ]

quotation :: Parser Token
quotation = label "quotation" $ do
    str <- L.lexeme spaceConsumer quotation'
    pure . Quotation $ Text.pack str

quotation' :: Parser String
quotation' = char '"' *> manyTill charLiteral (char '"')

parenthetical :: Parser Token
parenthetical = label "parenthetical" $ do
    str <- L.lexeme spaceConsumer parenthetical'
    pure . Parenthetical . Text.pack . init . drop 1 $ str

parenthetical' :: Parser String
parenthetical' = do
    str <- between (char '(') (char ')') q
    pure $ "(" ++ str ++ ")"
  where
    q = mconcat <$> some (c <|> parenthetical')
    c = (: []) <$> choice [contentChar, nonLineSpaceChar, char '"']

blockKey :: Parser Token
blockKey = label "block key" $ do
    fl <- upperChar
    rl <- many alphaNumChar
    pure . BlockKey . Text.pack $ fl : rl

ptext :: Parser Token
ptext = label "plain token" $ do
    str <- L.lexeme spaceConsumer $ some contentChar
    pure . PText . Text.pack $ str

docComment :: Parser Token
docComment = hidden $ do
    _ <- string "-- |" <* hspace
    str <- many (contentChar <|> nonLineSpaceChar)
    pure . DocComment . Text.pack $ str

comment :: Parser Token
comment = hidden $ do
    _ <- (string "--" <|> string "#") <* hspace
    str <- many (contentChar <|> nonLineSpaceChar)
    pure . Comment . Text.pack $ str

indentLevelBacktrack :: Parser () -> Parser Pos
indentLevelBacktrack sc = lookAhead $ do
    _ <- sc
    L.indentLevel

indented :: Parser () -> Pos -> Parser a -> Parser a
indented sc ref p =
    try $ do
        ind <- indentLevelBacktrack sc
        if ref == ind
            then sc *> p
            else mzero

flatBlock :: Parser () -> Parser a -> Parser a
flatBlock sc p = do
    ref <- indentLevelBacktrack sc
    indented sc ref p

docCommentBlock :: Parser (Maybe DocCommentBlock)
docCommentBlock = do
    lines <-
        many $ flatBlock indentationConsumer $ choice [docCommentLine, commentLine]
    if not (null (filterLines lines))
        then
            pure $
                Just
                    DocCommentBlock
                        { docCommentBlockLines = tokenContent <$> filterLines lines
                        }
        else pure Nothing
  where
    docCommentLine = docComment <* eol
    commentLine = comment <* eol
    isDocComment (DocComment _) = True
    isDocComment _ = False
    filterLines = dropWhile (not . isDocComment)

-- @since 2.16.0.0
anyToken :: Parser Token
anyToken =
    choice
        [ try equality
        , docComment
        , comment
        , quotation
        , parenthetical
        , ptext
        ]

anyNonCommentToken :: Parser Token
anyNonCommentToken =
    choice
        [ try equality
        , quotation
        , parenthetical
        , ptext
        ]

data ParsedEntityDef = ParsedEntityDef
    { parsedEntityDefComments :: [Text]
    , parsedEntityDefEntityName :: EntityNameHS
    , parsedEntityDefIsSum :: Bool
    , parsedEntityDefEntityAttributes :: [Attr]
    , parsedEntityDefFieldAttributes :: [([Token], Maybe Text)]
    , parsedEntityDefExtras :: M.Map Text [ExtraLine]
    , parsedEntityDefSpan :: Maybe SourceSpan
    }
    deriving (Show)

newtype DocCommentBlock = DocCommentBlock
    { docCommentBlockLines :: [Text]
    }
    deriving (Show)

docCommentBlockText :: DocCommentBlock -> Text
docCommentBlockText dcb = Text.unlines $ docCommentBlockLines dcb

data EntityHeader = EntityHeader
    { entityHeaderSum :: Bool
    , entityHeaderTableName :: Text
    , entityHeaderRemainingTokens :: [Token]
    , entityHeaderPos :: SourcePos
    , entityHeaderDocCommentBlock :: Maybe DocCommentBlock
    }
    deriving (Show)

data EntityBlock = EntityBlock
    { entityBlockEntityHeader :: EntityHeader
    , entityBlockMembers :: [Member]
    }
    deriving (Show)

entityBlockDocCommentBlock :: EntityBlock -> Maybe DocCommentBlock
entityBlockDocCommentBlock = entityHeaderDocCommentBlock . entityBlockEntityHeader

data ExtraBlockHeader = ExtraBlockHeader
    { extraBlockHeaderKey :: Text
    , extraBlockHeaderRemainingTokens :: [Token]
    , extraBlockHeaderPos :: SourcePos
    , extraBlockHeaderDocCommentBlock :: Maybe DocCommentBlock
    }
    deriving (Show)

data ExtraBlock = ExtraBlock
    { extraBlockExtraBlockHeader :: ExtraBlockHeader
    , extraBlockMembers :: NonEmpty Member
    }
    deriving (Show)

extraBlockDocCommentBlock :: ExtraBlock -> Maybe DocCommentBlock
extraBlockDocCommentBlock = extraBlockHeaderDocCommentBlock . extraBlockExtraBlockHeader

data BlockAttr = BlockAttr
    { blockAttrDocCommentBlock :: Maybe DocCommentBlock
    , blockAttrTokens :: [Token]
    , blockAttrPos :: SourcePos
    }
    deriving (Show)

data Member = MemberExtraBlock ExtraBlock | MemberBlockAttr BlockAttr
    deriving (Show)

-- | The source position at the beginning of the block's final line.
entityBlockEndPos :: EntityBlock -> SourcePos
entityBlockEndPos eb = case entityBlockMembers eb of
    [] -> entityBlockHeaderPos eb
    _ -> maximum $ fmap memberEndPos (entityBlockMembers eb)

entityBlockHeaderPos :: EntityBlock -> SourcePos
entityBlockHeaderPos = entityHeaderPos . entityBlockEntityHeader

-- | The source position at the beginning of the member's final line.
memberEndPos :: Member -> SourcePos
memberEndPos (MemberBlockAttr fs) = blockAttrPos fs
memberEndPos (MemberExtraBlock ex) = memberEndPos . NEL.last . extraBlockMembers $ ex

-- | Represents an entity member as a list of BlockAttrs
--
-- @since 2.16.0.0
memberBlockAttrs :: Member -> [BlockAttr]
memberBlockAttrs (MemberBlockAttr fs) = [fs]
memberBlockAttrs (MemberExtraBlock ex) = foldMap memberBlockAttrs . extraBlockMembers $ ex

entityBlockBlockAttrs :: EntityBlock -> [BlockAttr]
entityBlockBlockAttrs eb =
    foldMap f (entityBlockMembers eb)
  where
    f = \case
        MemberBlockAttr fs -> [fs]
        _ -> []

entityBlockExtraBlocks :: EntityBlock -> [ExtraBlock]
entityBlockExtraBlocks eb =
    foldMap f (entityBlockMembers eb)
  where
    f = \case
        MemberExtraBlock ex -> [ex]
        _ -> []

extraBlocksAsMap :: [ExtraBlock] -> M.Map Text [ExtraLine]
extraBlocksAsMap exs = M.fromList $ fmap asPair exs
  where
    asPair ex =
        (extraBlockHeaderKey . extraBlockExtraBlockHeader $ ex, extraLines ex)
    extraLines ex = foldMap asExtraLine (extraBlockMembers ex)
    asExtraLine (MemberBlockAttr fs) = [tokenContent <$> blockAttrTokens fs]
    asExtraLine _ = []

entityHeader :: Parser EntityHeader
entityHeader = do
    dcb <- docCommentBlock
    pos <- getSourcePos
    plus <- optional (char '+')
    en <- L.lexeme spaceConsumer blockKey
    rest <- L.lexeme spaceConsumer (many anyToken)
    pure
        EntityHeader
            { entityHeaderSum = isJust plus
            , entityHeaderTableName = tokenContent en
            , entityHeaderRemainingTokens = rest
            , entityHeaderPos = pos
            , entityHeaderDocCommentBlock = dcb
            }

extraBlock :: Parser Member
extraBlock = L.indentBlock indentationConsumer innerParser
  where
    mkExtraBlockMember (header, blockAttrs) =
        MemberExtraBlock
            ExtraBlock
                { extraBlockExtraBlockHeader = header
                , extraBlockMembers = ensureNonEmpty blockAttrs
                }
    ensureNonEmpty members = case NEL.nonEmpty members of
        Just nel -> nel
        Nothing -> error "unreachable" -- members is known to be non-empty
    innerParser = do
        header <- extraBlockHeader
        pure $ L.IndentSome Nothing (return . mkExtraBlockMember . (header,)) blockAttr

extraBlockHeader :: Parser ExtraBlockHeader
extraBlockHeader = do
    dcb <- docCommentBlock
    pos <- getSourcePos
    tn <- L.lexeme spaceConsumer blockKey
    rest <- L.lexeme spaceConsumer (many anyToken)
    pure $
        ExtraBlockHeader
            { extraBlockHeaderKey = tokenContent tn
            , extraBlockHeaderRemainingTokens = rest
            , extraBlockHeaderPos = pos
            , extraBlockHeaderDocCommentBlock = dcb
            }

blockAttr :: Parser Member
blockAttr = do
    ref <- indentLevelBacktrack indentationConsumer
    dcb <- docCommentBlock
    pos <- getSourcePos
    line <- indented indentationConsumer ref $ some anyNonCommentToken
    pure $
        MemberBlockAttr
            BlockAttr
                { blockAttrDocCommentBlock = dcb
                , blockAttrTokens = line
                , blockAttrPos = pos
                }

member :: Parser Member
member = try extraBlock <|> blockAttr

entityBlock :: Parser EntityBlock
entityBlock = L.indentBlock indentationConsumer innerParser
  where
    mkEntityBlock (header, members) =
        EntityBlock
            { entityBlockEntityHeader = header
            , entityBlockMembers = members
            }
    innerParser = do
        header <- entityHeader
        pure $ L.IndentMany Nothing (return . mkEntityBlock . (header,)) member

entitiesFromDocument :: Parser [EntityBlock]
entitiesFromDocument = many entityBlock <* hidden eof

parseEntities
    :: Text
    -> String
    -> Either (ParseErrorBundle String Void) [EntityBlock]
parseEntities fp = runParser entitiesFromDocument $ Text.unpack fp

toParsedEntityDef :: Maybe SourceLoc -> EntityBlock -> ParsedEntityDef
toParsedEntityDef mSourceLoc eb =
    ParsedEntityDef
        { parsedEntityDefComments = comments
        , parsedEntityDefEntityName = entityNameHS
        , parsedEntityDefIsSum = isSum
        , parsedEntityDefEntityAttributes = entityAttributes
        , parsedEntityDefFieldAttributes = parsedFieldAttributes
        , parsedEntityDefExtras = extras
        , parsedEntityDefSpan = mSpan
        }
  where
    comments =
        maybe
            []
            docCommentBlockLines
            (entityBlockDocCommentBlock eb)
    entityAttributes =
        tokenContent <$> (entityHeaderRemainingTokens . entityBlockEntityHeader) eb
    isSum = entityHeaderSum . entityBlockEntityHeader $ eb
    entityNameHS = EntityNameHS . entityHeaderTableName . entityBlockEntityHeader $ eb

    attributePair a = (blockAttrTokens a, docCommentBlockText <$> blockAttrDocCommentBlock a)
    parsedFieldAttributes = fmap attributePair (entityBlockBlockAttrs eb)

    extras = extraBlocksAsMap (entityBlockExtraBlocks eb)
    filepath = maybe "" locFile mSourceLoc
    relativeStartLine = maybe 0 locStartLine mSourceLoc
    relativeStartCol = maybe 0 locStartCol mSourceLoc
    mSpan =
        Just
            SourceSpan
                { spanFile = filepath
                , spanStartLine =
                    relativeStartLine + (unPos . sourceLine $ entityBlockHeaderPos eb)
                , spanEndLine = relativeStartLine + (unPos . sourceLine $ entityBlockEndPos eb)
                , spanStartCol =
                    relativeStartCol + (unPos . sourceColumn $ entityBlockHeaderPos eb)
                , spanEndCol = unPos . sourceColumn $ entityBlockEndPos eb
                }

parseSource :: Maybe SourceLoc -> Text -> [ParsedEntityDef]
parseSource mSourceLoc source =
    case parseEntities filepath (Text.unpack source) of
        Right blocks -> toParsedEntityDef mSourceLoc <$> blocks
        Left peb -> error $ errorBundlePretty peb
  where
    filepath = maybe "" locFile mSourceLoc
