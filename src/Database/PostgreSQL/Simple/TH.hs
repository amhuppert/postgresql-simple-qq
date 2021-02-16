{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Simple.TH
  ( psql,
    psqlStatement,
    SqlPart (..),
    queryQ,
  )
where

import Control.Exception
import qualified Data.Char as Char
import qualified Data.Text as Text
import Database.PostgreSQL.Simple
import qualified HeadedMegaparsec as HM
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified PostgresqlSyntax.Parsing as P
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Show
import Prelude hiding (some)

queryQ :: QuasiQuoter
queryQ =
  QuasiQuoter
    { quoteExp = queryQExp,
      quotePat = error "pat not implemented",
      quoteType = error "type not implemented",
      quoteDec = error "dec not implemented"
    }

queryQExp :: String -> Q Exp
queryQExp sql = [|(uncurry . query) $(psqlExp sql)|]

psql :: QuasiQuoter
psql =
  QuasiQuoter
    { quoteExp = psqlExp,
      quotePat = error "pat not implemented",
      quoteType = error "type not implemented",
      quoteDec = error "dec not implemented"
    }

-- Produces  (Query, <placeholder values>)
psqlExp :: String -> Q Exp
psqlExp sql = [|((fromString :: [Char] -> Query) sql', $(qValues))|]
  where
    parts = case parse psqlStatement "" (Text.pack sql) of
      Left err -> throw . ParseException . errorBundlePretty $ err
      Right ps -> ps

    (sql', placeholders) =
      buildAndCheck $ foldr step ([], [], []) parts
      where
        step (SyntaxFragment f) (fs, gs, ps) = (f : fs, f : gs, ps)
        step (Placeholder p) (fs, gs, ps) = ("?" : fs, "$0" : gs, p : ps)

        buildAndCheck (fs, gs, ps) =
          let psqlSimpleSql = Text.concat fs
              legalSql = Text.concat gs
           in (toString $ checkSyntax psqlSimpleSql legalSql, map toString ps)

    qValues = case placeholders of
      [x] -> [|Only $(varE . mkName $ x)|]
      xs -> tupE . map (varE . mkName) $ xs

    checkSyntax sqlToReturn sqlToCheck =
      case P.run (space' >> P.preparableStmt <* space') sqlToCheck of
        Left err -> throw . ParseException $ err
        Right _ -> sqlToReturn

    space' = HM.parse space

newtype ParseException = ParseException [Char]

instance Text.Show.Show ParseException where
  show (ParseException e) = e

instance Exception ParseException

type Parser = Parsec Void Text

data SqlPart
  = SyntaxFragment Text
  | Placeholder Text
  deriving (Show, Eq)

psqlStatement :: Parser [SqlPart]
psqlStatement =
  some $
    choice
      [ placeholder,
        singleQuoted,
        discard comment,
        fragment
      ]
  where
    discard p = p $> SyntaxFragment ""

placeholder :: Parser SqlPart
placeholder = do
  void $ char '?'
  Placeholder <$> haskellIdentifierName

singleQuoted :: Parser SqlPart
singleQuoted = do
  void $ char '\''
  inner <-
    Text.concat
      <$> manyTill quotedChar (char '\'')
  pure $ SyntaxFragment $ "'" <> inner <> "'"
  where
    quotedChar = string "\\'" <|> (Text.singleton <$> anySingleBut '\'')

haskellIdentifierName :: Parser Text
haskellIdentifierName = do
  takeWhile1P Nothing isIdentifierChar <?> "Haskell identifier name"
  where
    isIdentifierChar c = Char.isAlphaNum c || (c == '_') || c == '\''

fragment :: Parser SqlPart
fragment = SyntaxFragment <$> takeWhile1P Nothing (not . isFragmentBoundary)

isFragmentBoundary :: Char -> Bool
isFragmentBoundary c = c == '?' || c == '\'' || c == '/' || c == '-'

comment :: Parser SqlPart
comment = blockComment <|> lineComment

lineComment :: Parser SqlPart
lineComment = do
  void $ string "--"
  c <- takeWhileP Nothing (/= '\n')
  pure $ SyntaxFragment $ "--" <> c

blockComment :: Parser SqlPart
blockComment = do
  void $ string "/*"
  SyntaxFragment . Text.concat . ("/*" :) <$> chunks
  where
    chunks :: Parser [Text]
    chunks = do
      curr <- takeWhileP Nothing (/= '*')
      rest <- ((: []) <$> string "*/") <|> ((:) <$> string "*" <*> chunks)
      pure (curr : rest)
