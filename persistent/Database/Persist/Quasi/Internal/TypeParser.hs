{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Database.Persist.Quasi.Internal.TypeParser
  ( TypeExpr
  , typeExpr
  , typeExprContent
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data TypeExpr
  = TypeApplication TypeConstructor [TypeExpr]
  | TypelevelString String
  deriving (Show, Eq)

data TypeConstructor
  = ListConstructor
  | TypeConstructor String
  deriving (Show, Eq)

typeExpr :: (MonadParsec e String) m => m TypeExpr
typeExpr = choice [ listType
                  , typeApplication
                  , typelevelString
                  , whitespaceBetween '(' typeExpr ')'
                  ]

-- parses "normal" type constructors, including nullary ones.
typeConstructor :: (MonadParsec e String) m => m TypeConstructor
typeConstructor = do
  first <- upperChar
  rest <- many alphaNumChar
  pure $ TypeConstructor (first : rest)

whitespaceBetween :: (MonadParsec e String) m => Char -> m a -> Char -> m a
whitespaceBetween ldelim p rdelim =
  between (char ldelim *> optional hspace) (optional hspace *> char rdelim) p

typeApplication :: (MonadParsec e String) m => m TypeExpr
typeApplication = do
  tc <- typeConstructor <* optional hspace
  args <- many (typeExpr <* optional hspace)
  pure $ TypeApplication tc args

typelevelString :: (MonadParsec e String) m => m TypeExpr
typelevelString = do
  s <- char '"' *> manyTill L.charLiteral (char '"')
  pure $ TypelevelString s

listType :: (MonadParsec e String) m => m TypeExpr
listType = do
  t <- whitespaceBetween '[' typeExpr ']'
  pure $ TypeApplication ListConstructor [t]

typeExprContent :: TypeExpr -> Text
typeExprContent = typeExprContent' False

typeExprContent' :: Bool -> TypeExpr -> Text
typeExprContent' wrapped = \case
  TypelevelString s -> mconcat [ "\""
                               , T.pack s
                               ,"\""
                               ]
  TypeApplication tc exps -> typeApplicationContent tc exps wrapped
  where
    typeArgsListContent :: [TypeExpr] -> Text
    typeArgsListContent exps = T.intercalate " " $ fmap (typeExprContent' True) exps

    typeApplicationContent :: TypeConstructor -> [TypeExpr] -> Bool -> Text
    typeApplicationContent ListConstructor args _ = mconcat [ "["
                                                            , typeArgsListContent args
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
              , typeArgsListContent exps
              ]
