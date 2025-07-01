{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Database.Persist.Quasi.Internal.TypeParser
-- todo dtp:
--  ( TypeExpr
--  , typeExpr
--  , typeExprContent
--  ) where
  where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data TypeExpr
  = TypeApplication TypeConstructor [TypeExpr]
  | TypeLitString String
  | TypeLitInt String
  | TypeLitPromotedConstructor TypeConstructor
  deriving (Show, Eq)

data TypeConstructor
  = ListConstructor
  | TypeConstructor String
  deriving (Show, Eq)

-- Parses top-level type expressions
typeExpr :: (MonadParsec e String) m => m TypeExpr
typeExpr = label "type expression" $ do
  choice [ listType
         , typeApplication
         , whitespaceBetween '(' typeExpr ')'
         , typeLitString
         , typeLitInt
         , typeLitPromotedConstructor
         ]

-- Parses arguments to a type constructor, in which further
-- applications must be ()-delimited.
typeArgExpr :: (MonadParsec e String) m => m TypeExpr
typeArgExpr = label "type expression" $ do
  choice [ listType
         , nullaryTypeApplication
         , whitespaceBetween '(' typeExpr ')'
         , typeLitString
         , typeLitInt
         , typeLitPromotedConstructor
         ]

-- parses "normal" type constructors, INCLUDING nullary ones.
typeConstructor :: (MonadParsec e String) m => m TypeConstructor
typeConstructor = do
  first <- upperChar
  rest <- many $ choice [alphaNumChar, char '.', char '\'']
  pure $ TypeConstructor (first : rest)

whitespaceBetween :: (MonadParsec e String) m => Char -> m a -> Char -> m a
whitespaceBetween ldelim p rdelim =
  between (char ldelim *> optional hspace) (optional hspace *> char rdelim) p

typeApplication :: (MonadParsec e String) m => m TypeExpr
typeApplication = do
  tc <- typeConstructor <* optional hspace
  args <- many (typeArgExpr <* optional hspace)
  pure $ TypeApplication tc args

-- ONLY parses applications of nullary constructors.
nullaryTypeApplication :: (MonadParsec e String) m => m TypeExpr
nullaryTypeApplication = do
  tc <- typeConstructor <* optional hspace
  pure $ TypeApplication tc []

typeLitString :: (MonadParsec e String) m => m TypeExpr
typeLitString = do
  s <- char '"' *> manyTill L.charLiteral (char '"')
  pure $ TypeLitString s

typeLitInt :: (MonadParsec e String) m => m TypeExpr
typeLitInt = TypeLitInt <$> some digitChar

typeLitPromotedConstructor :: (MonadParsec e String) m => m TypeExpr
typeLitPromotedConstructor = do
  _ <- char '\''
  _ <- optional hspace
  TypeLitPromotedConstructor <$> typeConstructor

listType :: (MonadParsec e String) m => m TypeExpr
listType = do
  t <- whitespaceBetween '[' typeExpr ']'
  pure $ TypeApplication ListConstructor [t]

typeExprContent :: TypeExpr -> Text
typeExprContent = typeExprContent' False

typeExprContent' :: Bool -> TypeExpr -> Text
typeExprContent' wrapped = \case
  TypeLitString s -> mconcat [ "\""
                               , T.pack s
                               ,"\""
                               ]
  TypeLitInt s -> T.pack s
  TypeLitPromotedConstructor (TypeConstructor s) -> T.pack ('\'' : s)
  TypeLitPromotedConstructor ListConstructor -> T.pack $ "'[]"
  TypeApplication tc exps -> typeApplicationContent tc exps wrapped
  where
    typeArgsListContent :: Bool -> [TypeExpr] -> Text
    typeArgsListContent wrapped exps = T.intercalate " " $ fmap (typeExprContent' wrapped) exps

    typeApplicationContent :: TypeConstructor -> [TypeExpr] -> Bool -> Text
    typeApplicationContent ListConstructor args _ = mconcat [ "["
                                                            , typeArgsListContent False args
                                                            , "]"
                                                            ]
    typeApplicationContent (TypeConstructor s) [] _ = T.pack s
    typeApplicationContent (TypeConstructor s) exps True =
      mconcat [ "("
              , typeApplicationContent (TypeConstructor s) exps False
              , ")"
              ]
    typeApplicationContent (TypeConstructor s) exps False =
      mconcat [ T.pack s
              , " "
              , typeArgsListContent True exps
              ]
