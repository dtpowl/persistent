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
    , parsedEntityDefFieldComments
    , parsedEntityDefExtras
    , parsedEntityDefSpan
    , parseSource
    , memberBlockAttrs
    ) where

import Prelude hiding (lines, unlines)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text, pack, unlines, unpack)
import Data.Void
import Database.Persist.Types
import Database.Persist.Types.Span
import Language.Haskell.TH.Syntax (Lift)
import Replace.Megaparsec (sepCap)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- | Source location: file and line/col information. This is half of a 'Span'.
data SourceLoc = SourceLoc
    { locFile :: Text
    , locStartLine :: Int
    , locStartCol :: Int
    }
    deriving (Show, Lift)

data Token
    = DocComment Text
    | Comment Text
    | Quotation Text
    | Equality Text Text
    | Parenthetical Text
    | BlockKey Text
    | PText Text
    deriving (Eq, Ord, Show)

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

spaceConsumerN :: Parser ()
spaceConsumerN =
    L.space
        space1
        (L.skipLineComment "--")
        empty

escapedParen :: Parser Char
escapedParen =
    char '\\'
        *> choice
            [ char '('
            , char ')'
            ]

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
        , try escapedParen
        , char '\\'
        ]

nonLineSpaceChar :: Parser Char
nonLineSpaceChar = choice [char ' ', char '\t']

-- This is a replacement for `Text.Megaparsec.Char.Lexer.charLiteral`;
-- it does nearly the same thing but additionally supports escaped parentheses.
charLiteral :: Parser Char
charLiteral = label "literal character" $ do
    r <- lookAhead (count' 1 10 anySingle)
    case lexChar r of
        Just (c, r') -> c <$ skipCount (length r - length r') anySingle
        Nothing -> unexpected (errorTokens r)
  where
    errorTokens ts = Tokens $
        case NEL.nonEmpty ts of
            Just nel -> NEL.head nel :| []
            Nothing -> error "unreachable" -- ts is known to be non-empty
    lexChar "" =
        Nothing
    lexChar ('\\' : '(' : rest) =
        Just ('(', rest)
    lexChar ('\\' : ')' : rest) =
        Just (')', rest)
    lexChar ('\\' : '\\' : rest) =
        Just ('\\', rest)
    lexChar ('\\' : '\'' : rest) =
        Just ('\'', rest)
    lexChar ('\\' : '\"' : rest) =
        Just ('\"', rest)
    lexChar (a : rest) =
        Just (a, rest)

equality :: Parser Token
equality = label "equality expression" $ do
    L.lexeme spaceConsumer $ do
        lhs <- some contentChar
        _ <- char '='
        rhs <-
            choice
                [ try quotation'
                , try sqlLiteral
                , try parenthetical'
                , some $ contentChar <|> char '(' <|> char ')'
                ]
        pure $ Equality (pack lhs) (pack rhs)
--  where
--    quotationInner = do
--        str <- quotation'
--        pure $ "\"" <> str <> "\""

sqlTypeName :: Parser String
sqlTypeName = some $ choice [ alphaNumChar
                            , char '_'
                            ]

sqlLiteral :: Parser String
sqlLiteral = label "SQL literal" $ do
  quote <- L.lexeme spaceConsumer $ char '\'' *> manyTill charLiteral (char '\'')
  st <- optional $ do
    colons <- string "::"
    tn <- sqlTypeName
    pure $ colons <> tn
  pure $ mconcat [ "'"
                 , quote
                 , "'"
                 , maybe "" id st
                 ]

quotation :: Parser Token
quotation = label "quotation" $ do
    str <- L.lexeme spaceConsumer quotation'
    pure . Quotation $ pack str

quotation' :: Parser String
quotation' = char '"' *> manyTill charLiteral (char '"')

parenthetical :: Parser Token
parenthetical = label "parenthetical" $ do
    str <- L.lexeme spaceConsumer parenthetical'
    pure . Parenthetical . pack . init . drop 1 $ str

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
    pure . BlockKey . pack $ fl : rl

ptext :: Parser Token
ptext = label "plain token" $ do
    str <- L.lexeme spaceConsumer $ some contentChar
    pure . PText . pack $ str

docComment :: Parser Token
docComment = label "doc comment" $ do
    _ <- string "-- |" <* hspace
    str <- many (contentChar <|> nonLineSpaceChar)
    pure . DocComment . pack $ str

comment :: Parser Token
comment = label "comment" $ do
    _ <- (string "--" <|> string "#") <* hspace
    str <- many (contentChar <|> nonLineSpaceChar)
    pure . Comment . pack $ str

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

data ParsedEntityDef = ParsedEntityDef
    { parsedEntityDefComments :: [Text]
    , parsedEntityDefEntityName :: EntityNameHS
    , parsedEntityDefIsSum :: Bool
    , parsedEntityDefEntityAttributes :: [Attr]
    , parsedEntityDefFieldAttributes :: [[Token]]
    , parsedEntityDefFieldComments :: [Maybe Text]
    , parsedEntityDefExtras :: M.Map Text [ExtraLine]
    , parsedEntityDefSpan :: Maybe Span
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

-- | The source position at the beginning of the block's final line.
entityBlockEndPos :: EntityBlock -> SourcePos
entityBlockEndPos eb = case entityBlockMembers eb of
    [] -> entityBlockHeaderPos eb
    _ -> maximum $ fmap memberEndPos (entityBlockMembers eb)

entityBlockHeaderPos :: EntityBlock -> SourcePos
entityBlockHeaderPos = entityHeaderPos . entityBlockEntityHeader

-- | The source position at the beginning of the member's first line.
memberPos :: Member -> SourcePos
memberPos (MemberBlockAttr fs) = blockAttrPos fs
memberPos (MemberExtraBlock ex) = extraBlockHeaderPos . extraBlockExtraBlockHeader $ ex

-- | The source position at the beginning of the member's final line.
memberEndPos :: Member -> SourcePos
memberEndPos (MemberBlockAttr fs) = blockAttrPos fs
memberEndPos (MemberExtraBlock ex) = memberEndPos . NEL.last . extraBlockMembers $ ex

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

docCommentBlock :: Parser DocCommentBlock
docCommentBlock = do
    pos <- getSourcePos
    dc <- hspace *> docComment <* space
    cb <- many $ hspace *> (docComment <|> comment) <* space
    pure $
        DocCommentBlock
            { docCommentBlockLines = [tokenContent dc] <> map tokenContent cb
            , docCommentBlockPos = pos
            }

docCommentsFromDocument :: Parser [DocCommentBlock]
docCommentsFromDocument =
    fmap snd . extractFrom <$> findAllDcBlocks
  where
    findAllDcBlocks = sepCap (match docCommentBlock)
    extractFrom = \case
        [] -> []
        Right x : xs -> x : extractFrom xs
        Left _ : xs -> extractFrom xs

docCommentBlockText :: DocCommentBlock -> Text
docCommentBlockText dcb = unlines $ docCommentBlockLines dcb

associateDocComments :: [DocCommentBlock] -> [EntityBlock] -> [EntityBlock]
associateDocComments _ [] = []
associateDocComments [] es = es
associateDocComments (dc : rest) es = associateDocComments rest (applyDocToBestEntity dc es)

commentIsPositionedFor :: DocCommentBlock -> SourcePos -> Bool
commentIsPositionedFor dc sp =
    (sourceLine dcpos < sourceLine sp) && (sourceColumn dcpos <= sourceColumn sp)
  where
    dcpos = docCommentBlockPos dc

applyDocToBestEntity :: DocCommentBlock -> [EntityBlock] -> [EntityBlock]
applyDocToBestEntity _ [] = []
applyDocToBestEntity dc (e : rest) =
    if commentIsPositionedFor dc (entityBlockEndPos e)
        then applyDocToEntity dc e : rest
        else e : applyDocToBestEntity dc rest

applyDocToEntity :: DocCommentBlock -> EntityBlock -> EntityBlock
applyDocToEntity dc e =
    if commentIsPositionedFor dc (entityBlockHeaderPos e)
        then e{entityBlockDocCommentBlock = Just dc}
        else e{entityBlockMembers = applyDocToBestMember dc (entityBlockMembers e)}

applyDocToBestMember :: DocCommentBlock -> [Member] -> [Member]
applyDocToBestMember _ [] = []
applyDocToBestMember dc (m : rest) =
    if commentIsPositionedFor dc (memberPos m)
        then applyDocToMember dc m : rest
        else m : applyDocToBestMember dc rest

applyDocToMember :: DocCommentBlock -> Member -> Member
applyDocToMember dc (MemberBlockAttr fs) = MemberBlockAttr fs{blockAttrDocCommentBlock = Just dc}
applyDocToMember dc (MemberExtraBlock ex) =
    if commentIsPositionedFor
        dc
        (extraBlockHeaderPos . extraBlockExtraBlockHeader $ ex)
        then MemberExtraBlock ex{extraBlockDocCommentBlock = Just dc}
        else MemberExtraBlock ex{extraBlockMembers = appliedNel}
  where
    appliedList = applyDocToBestMember dc $ NEL.toList (extraBlockMembers ex)
    appliedNel = case appliedList of
        a : rest -> a :| rest
        [] -> error "unreachable" -- appliedList is known to be non-empty

parseEntities
    :: Text -> String -> Either (ParseErrorBundle String Void) [EntityBlock]
parseEntities fp s = do
    entities <- runParser entitiesFromDocument (unpack fp) s
    docComments <- runParser docCommentsFromDocument (unpack fp) s
    pure $ associateDocComments docComments entities

toParsedEntityDef :: Maybe SourceLoc -> EntityBlock -> ParsedEntityDef
toParsedEntityDef mSourceLoc eb =
    ParsedEntityDef
        { parsedEntityDefComments = comments
        , parsedEntityDefEntityName = entityNameHS
        , parsedEntityDefIsSum = isSum
        , parsedEntityDefEntityAttributes = entityAttributes
        , parsedEntityDefFieldAttributes = parsedFieldAttributes
        , parsedEntityDefFieldComments = parsedFieldComments
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

    parsedFieldAttributes = fmap blockAttrTokens (entityBlockBlockAttrs eb)
    parsedFieldComments =
        fmap docCommentBlockText
            <$> fmap blockAttrDocCommentBlock (entityBlockBlockAttrs eb)

    extras = extraBlocksAsMap (entityBlockExtraBlocks eb)
    filepath = maybe "" locFile mSourceLoc
    relativeStartLine = maybe 0 locStartLine mSourceLoc
    relativeStartCol = maybe 0 locStartCol mSourceLoc
    mSpan =
        Just
            Span
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
    case parseEntities filepath (unpack source) of
        Right blocks -> toParsedEntityDef mSourceLoc <$> blocks
        Left peb -> error $ errorBundlePretty peb
  where
    filepath = maybe "" locFile mSourceLoc
