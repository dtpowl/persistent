{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
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
    , ParseResult
    , CumulativeParseResult (..)
    , toCumulativeParseResult
    , renderErrors
    , runConfiguredParser
    ) where

import Control.Monad.Trans.State
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import Database.Persist.Types
import Database.Persist.Types.SourceSpan
import Language.Haskell.TH.Syntax (Lift)
import Replace.Megaparsec (sepCap)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- We'll augment the parser with extra state to accumulate comments seen during parsing.
-- Comments are lexed as whitespace, but will be used to generate documentation later.
type ExtraState = [(SourcePos, CommentToken)]

type Parser a =
    StateT
        ExtraState
        (ParsecT Void String (Either (ParseErrorBundle String Void)))
        a
type InternalParseResult a =
    Either (ParseErrorBundle String Void) (a, ExtraState)
type ParseResult a = Either (ParseErrorBundle String Void) a

-- | Run a parser using a provided ExtraState
-- @since 2.16.0.0
runConfiguredParser
    :: ExtraState
    -> Parser a
    -> String
    -> String
    -> InternalParseResult a
runConfiguredParser acc parser fp s = do
    (_internalState, parseResult) <-
        runParserT' (runStateT parser acc) initialInternalState
    parseResult
  where
    initialSourcePos =
        SourcePos
            { sourceName = fp
            , sourceLine = pos1
            , sourceColumn = pos1
            }
    initialPosState =
        PosState
            { pstateInput = s
            , pstateOffset = 0
            , pstateSourcePos = initialSourcePos
            , -- for legacy compatibility, we treat each tab as a single unit of whitespace
              pstateTabWidth = pos1
            , pstateLinePrefix = ""
            }
    initialInternalState =
        State
            { stateInput = s
            , stateOffset = 0
            , statePosState = initialPosState
            , stateParseErrors = []
            }

-- @since 2.16.0.0
data CumulativeParseResult a = CumulativeParseResult
    { cumulativeErrors :: [ParseErrorBundle String Void]
    , cumulativeData :: a
    }

-- | Populates a CumulativeParseResult with a single error or datum
-- @since 2.16.0.0
toCumulativeParseResult
    :: (Monoid a)
    => ParseResult a
    -> CumulativeParseResult a
toCumulativeParseResult (Left peb) =
    CumulativeParseResult
        { cumulativeErrors = [peb]
        , cumulativeData = mempty
        }
toCumulativeParseResult (Right res) =
    CumulativeParseResult
        { cumulativeErrors = []
        , cumulativeData = res
        }

-- | Converts the errors in a CumulativeParseResult to a String
-- @since 2.16.0.0
renderErrors :: CumulativeParseResult a -> Maybe String
renderErrors cpr =
    case cumulativeErrors cpr of
        [] -> Nothing
        pebs -> Just $ intercalate "\n" $ fmap errorBundlePretty pebs

instance (Semigroup a) => Semigroup (CumulativeParseResult a) where
    (<>) l r =
        CumulativeParseResult
            { cumulativeErrors = cumulativeErrors l ++ cumulativeErrors r
            , cumulativeData = cumulativeData l <> cumulativeData r
            }

instance (Monoid a) => Monoid (CumulativeParseResult a) where
    mempty =
        CumulativeParseResult
            { cumulativeErrors = mempty
            , cumulativeData = mempty
            }

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
    = Quotation Text
    | Equality Text Text
    | Parenthetical Text
    | BlockKey Text
    | PText Text
    deriving (Eq, Ord, Show)

-- @since 2.16.0.0
data CommentToken
    = DocComment Text
    | Comment Text
    deriving (Eq, Ord, Show)

-- | Converts a token into a Text representation for second-stage parsing or presentation to the user
--
-- @since 2.16.0.0
tokenContent :: Token -> Text
tokenContent = \case
    Quotation s -> s
    Equality l r -> mconcat [l, "=", r]
    Parenthetical s -> s
    PText s -> s
    BlockKey s -> s

commentContent :: CommentToken -> Text
commentContent = \case
    Comment s -> s
    DocComment s -> s

docComment :: Parser (SourcePos, CommentToken)
docComment = do
    pos <- getSourcePos
    content <- string "-- |" *> hspace *> takeWhileP (Just "character") (/= '\n')
    pure (pos, DocComment (Text.pack content))

comment :: Parser (SourcePos, CommentToken)
comment = do
    pos <- getSourcePos
    content <-
        (string "--" <|> string "#")
            *> hspace
            *> takeWhileP (Just "character") (/= '\n')
    pure (pos, Comment (Text.pack content))

skipComment :: Parser ()
skipComment = do
    content <- docComment <|> comment
    comments <- get
    put $ comments ++ [content]
    pure ()

spaceConsumer :: Parser ()
spaceConsumer =
    L.space
        hspace1
        skipComment
        empty

spaceConsumerN :: Parser ()
spaceConsumerN =
    L.space
        space1
        skipComment
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
        , char ','
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

-- @since 2.16.0.0
anyToken :: Parser Token
anyToken =
    choice
        [ try equality
        , quotation
        , parenthetical
        , ptext
        ]

class Block a where
    blockFirstPos :: a -> SourcePos
    blockMembers :: a -> [Member]
    blockSetMembers :: [Member] -> a -> a
    blockSetNELMembers :: NonEmpty Member -> a -> a
    blockSetDocCommentBlock :: Maybe DocCommentBlock -> a -> a

instance Block EntityBlock where
    blockFirstPos = entityHeaderPos . entityBlockEntityHeader
    blockMembers = entityBlockMembers
    blockSetMembers ms b = b{entityBlockMembers = ms}
    blockSetNELMembers nel = blockSetMembers (NEL.toList nel)
    blockSetDocCommentBlock dcb b = b{entityBlockDocCommentBlock = dcb}

instance Block ExtraBlock where
    blockFirstPos = extraBlockHeaderPos . extraBlockExtraBlockHeader
    blockMembers = NEL.toList . extraBlockMembers
    blockSetMembers ms b = case NEL.nonEmpty ms of
        Nothing -> b
        Just nel -> blockSetNELMembers nel b
    blockSetNELMembers nel b = b{extraBlockMembers = nel}
    blockSetDocCommentBlock dcb b = b{extraBlockDocCommentBlock = dcb}

blockLastPos :: (Block a) => a -> SourcePos
blockLastPos b = case blockMembers b of
    [] -> blockFirstPos b
    members -> maximum $ fmap memberEndPos members

blockBlockAttrs :: (Block a) => a -> [BlockAttr]
blockBlockAttrs eb =
    foldMap f (blockMembers eb)
  where
    f = \case
        MemberBlockAttr fs -> [fs]
        _ -> []

blockExtraBlocks :: (Block a) => a -> [ExtraBlock]
blockExtraBlocks eb =
    foldMap f (blockMembers eb)
  where
    f = \case
        MemberExtraBlock ex -> [ex]
        _ -> []

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

data DocCommentBlock = DocCommentBlock
    { docCommentBlockLines :: [Text]
    , docCommentBlockPos :: SourcePos
    }
    deriving (Show)

data EntityHeader = EntityHeader
    { entityHeaderSum :: Bool
    , entityHeaderTableName :: Text
    , entityHeaderRemainingTokens :: [Token]
    , entityHeaderPos :: SourcePos
    }
    deriving (Show)

data EntityBlock = EntityBlock
    { entityBlockDocCommentBlock :: Maybe DocCommentBlock
    , entityBlockEntityHeader :: EntityHeader
    , entityBlockMembers :: [Member]
    }
    deriving (Show)

data ExtraBlockHeader = ExtraBlockHeader
    { extraBlockHeaderKey :: Text
    , extraBlockHeaderRemainingTokens :: [Token]
    , extraBlockHeaderPos :: SourcePos
    }
    deriving (Show)

data ExtraBlock = ExtraBlock
    { extraBlockDocCommentBlock :: Maybe DocCommentBlock
    , extraBlockExtraBlockHeader :: ExtraBlockHeader
    , extraBlockMembers :: NonEmpty Member
    }
    deriving (Show)

data BlockAttr = BlockAttr
    { blockAttrDocCommentBlock :: Maybe DocCommentBlock
    , blockAttrTokens :: [Token]
    , blockAttrPos :: SourcePos
    }
    deriving (Show)

data Member = MemberExtraBlock ExtraBlock | MemberBlockAttr BlockAttr
    deriving (Show)

-- | The source position at the beginning of the member's first line.
memberPos :: Member -> SourcePos
memberPos (MemberBlockAttr fs) = blockAttrPos fs
memberPos (MemberExtraBlock ex) = extraBlockHeaderPos . extraBlockExtraBlockHeader $ ex

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
    pos <- getSourcePos
    plus <- optional (char '+')
    en <- hspace *> L.lexeme spaceConsumer blockKey
    rest <- L.lexeme spaceConsumer (many anyToken)
    pure
        EntityHeader
            { entityHeaderSum = isJust plus
            , entityHeaderTableName = tokenContent en
            , entityHeaderRemainingTokens = rest
            , entityHeaderPos = pos
            }

extraBlock :: Parser Member
extraBlock = L.indentBlock spaceConsumerN innerParser
  where
    mkExtraBlockMember (header, blockAttrs) =
        MemberExtraBlock
            ExtraBlock
                { extraBlockExtraBlockHeader = header
                , extraBlockMembers = ensureNonEmpty blockAttrs
                , extraBlockDocCommentBlock = Nothing
                }
    ensureNonEmpty members = case NEL.nonEmpty members of
        Just nel -> nel
        Nothing -> error "unreachable" -- members is known to be non-empty
    innerParser = do
        header <- extraBlockHeader
        pure $ L.IndentSome Nothing (return . mkExtraBlockMember . (header,)) blockAttr

extraBlockHeader :: Parser ExtraBlockHeader
extraBlockHeader = do
    pos <- getSourcePos
    tn <- L.lexeme spaceConsumer blockKey
    rest <- L.lexeme spaceConsumer (many anyToken)
    pure $
        ExtraBlockHeader
            { extraBlockHeaderKey = tokenContent tn
            , extraBlockHeaderRemainingTokens = rest
            , extraBlockHeaderPos = pos
            }

blockAttr :: Parser Member
blockAttr = do
    pos <- getSourcePos
    line <- some anyToken
    pure $
        MemberBlockAttr
            BlockAttr
                { blockAttrDocCommentBlock = Nothing
                , blockAttrTokens = line
                , blockAttrPos = pos
                }

member :: Parser Member
member = try extraBlock <|> blockAttr

entityBlock :: Parser EntityBlock
entityBlock = L.indentBlock spaceConsumerN innerParser
  where
    mkEntityBlock (header, members) =
        EntityBlock
            { entityBlockEntityHeader = header
            , entityBlockMembers = members
            , entityBlockDocCommentBlock = Nothing
            }
    innerParser = do
        header <- entityHeader
        pure $ L.IndentMany Nothing (return . mkEntityBlock . (header,)) member

entitiesFromDocument :: Parser [EntityBlock]
entitiesFromDocument = many entityBlock

docCommentBlockText :: DocCommentBlock -> Text
docCommentBlockText dcb = Text.unlines $ docCommentBlockLines dcb

isDocComment :: CommentToken -> Bool
isDocComment tok = case tok of
    DocComment _ -> True
    _ -> False

docCommentBlockFromPositionedTokens
    :: [(SourcePos, CommentToken)] -> Maybe DocCommentBlock
docCommentBlockFromPositionedTokens ptoks =
    case NEL.nonEmpty ptoks of
        Nothing -> Nothing
        Just nel ->
            Just $
                DocCommentBlock
                    { docCommentBlockLines = NEL.toList $ fmap (commentContent . snd) nel
                    , docCommentBlockPos = fst $ NEL.head nel
                    }

associateCommentLines
    :: [(SourcePos, CommentToken)] -> [EntityBlock] -> [EntityBlock]
associateCommentLines _ [] = []
associateCommentLines [] es = es
associateCommentLines cls (eh : et) =
    applyCommentLinesToBlock candidateLines eh
        : associateCommentLines remainingLines et
  where
    dcLines = dropWhile (not . isDocComment . snd) cls
    candidateLines =
        takeWhile
            (\(spos, _) -> sourceLine spos < sourceLine (blockLastPos eh))
            dcLines
    remainingLines = drop (length candidateLines) dcLines

-- | Accepts a list of (position, comment) pairs and associates them with the
-- block and its members.
applyCommentLinesToBlock :: (Block a) => [(SourcePos, CommentToken)] -> a -> a
applyCommentLinesToBlock [] a = a
applyCommentLinesToBlock cls a = blockSetDocCommentBlock dcb $ blockSetMembers commentedMembers a
  where
    startLine = sourceLine $ blockFirstPos a
    headerLines = takeWhile (\(spos, _t) -> sourceLine spos < startLine) cls
    memberLines = dropWhile (\(spos, _t) -> sourceLine spos <= startLine) cls
    dcb = docCommentBlockFromPositionedTokens headerLines
    commentedMembers = associateCommentLinesWithMembers memberLines (blockMembers a)

associateCommentLinesWithMembers
    :: [(SourcePos, CommentToken)] -> [Member] -> [Member]
associateCommentLinesWithMembers [] ms = ms
associateCommentLinesWithMembers _ [] = []
associateCommentLinesWithMembers cls ms@(mh : mt) = do
    applyCommentLinesToMember candidateLines mh
        : associateCommentLinesWithMembers remainingLines mt
  where
    -- we must ignore comments that share a line number with any member
    membersLinePoses = Set.fromDistinctAscList $ fmap (sourceLine . memberPos) ms
    filteredLines = filter (\(pos, _t) -> Set.notMember (sourceLine pos) membersLinePoses) cls
    dcLines = dropWhile (not . isDocComment . snd) filteredLines
    candidateLines =
        takeWhile (\(spos, _) -> sourceLine spos < sourceLine (memberEndPos mh)) dcLines
    remainingLines = drop (length candidateLines) dcLines

applyCommentLinesToMember :: [(SourcePos, CommentToken)] -> Member -> Member
applyCommentLinesToMember cls m = case m of
    MemberBlockAttr a -> MemberBlockAttr $ applyCommentLinesToBlockAttr cls a
    MemberExtraBlock b -> MemberExtraBlock $ applyCommentLinesToBlock cls b

applyCommentLinesToBlockAttr
    :: [(SourcePos, CommentToken)] -> BlockAttr -> BlockAttr
applyCommentLinesToBlockAttr [] a = a
applyCommentLinesToBlockAttr cls a = a{blockAttrDocCommentBlock = dcb}
  where
    ls =
        takeWhile
            (\(spos, _t) -> sourceLine spos < sourceLine (blockAttrPos a))
            cls
    dcb = docCommentBlockFromPositionedTokens ls

parseEntities
    :: Text
    -> String
    -> ParseResult [EntityBlock]
parseEntities fp s = do
    (entities, comments) <-
        runConfiguredParser [] entitiesFromDocument (Text.unpack fp) s
    pure $ associateCommentLines comments entities

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
    parsedFieldAttributes = fmap attributePair (blockBlockAttrs eb)

    extras = extraBlocksAsMap (blockExtraBlocks eb)
    filepath = maybe "" locFile mSourceLoc
    relativeStartLine = maybe 0 locStartLine mSourceLoc
    relativeStartCol = maybe 0 locStartCol mSourceLoc
    mSpan =
        Just
            SourceSpan
                { spanFile = filepath
                , spanStartLine =
                    relativeStartLine + (unPos . sourceLine $ blockFirstPos eb)
                , spanEndLine = relativeStartLine + (unPos . sourceLine $ blockLastPos eb)
                , spanStartCol =
                    relativeStartCol + (unPos . sourceColumn $ blockFirstPos eb)
                , spanEndCol = unPos . sourceColumn $ blockLastPos eb
                }

parseSource :: Maybe SourceLoc -> Text -> ParseResult [ParsedEntityDef]
parseSource mSourceLoc source =
    case parseEntities filepath (Text.unpack source) of
        Right blocks -> Right (toParsedEntityDef mSourceLoc <$> blocks)
        Left peb -> Left peb
  where
    filepath = maybe "" locFile mSourceLoc
