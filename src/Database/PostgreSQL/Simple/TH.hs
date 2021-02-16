{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Simple.TH
  ( psql,
    psqlStatement,
    SqlPart (..),
  )
where

import qualified Data.Char as Char
import qualified Data.Text as Text
import Database.PostgreSQL.Simple
import qualified HeadedMegaparsec as HM
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified PostgresqlSyntax.Parsing as P
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
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
      Left err -> error (show err)
      Right ps -> ps

    (sql' :: [Char], placeholders) =
      bimap
        (toString . checkSyntax . Text.concat)
        (map toString)
        $ foldr step ([], []) parts
      where
        step (SyntaxFragment f) (fs, ps) = (f : fs, ps)
        step (Placeholder p) (fs, ps) = ("?" : fs, p : ps)

    qValues = case placeholders of
      [x] -> [|Only $(varE . mkName $ x)|]
      xs -> tupE . map (varE . mkName) $ xs

    checkSyntax s = case P.run (space' >> P.preparableStmt <* space') s of
      Left err -> error $ "SQL Syntax Error: " <> toText err
      Right _ -> s

    space' = HM.parse space

type Parser = Parsec Void Text

data SqlPart
  = SyntaxFragment Text
  | Placeholder Text
  deriving (Show, Eq)

psqlStatement :: Parsec Void Text [SqlPart]
psqlStatement = some (choice [placeholder, singleQuoted, comment, fragment])

placeholder :: Parsec Void Text SqlPart
placeholder = do
  char '?'
  Placeholder <$> haskellIdentifierName

singleQuoted :: Parsec Void Text SqlPart
singleQuoted = do
  char '\''
  inner <-
    Text.concat
      <$> manyTill quotedChar (char '\'')
  pure $ SyntaxFragment $ "'" <> inner <> "'"
  where
    quotedChar = string "\\'" <|> (Text.singleton <$> anySingleBut '\'')

haskellIdentifierName :: Parsec Void Text Text
haskellIdentifierName = do
  takeWhile1P Nothing isIdentifierChar <?> "Haskell identifier name"
  where
    isIdentifierChar c = Char.isAlphaNum c || (c == '_') || c == '\''

fragment :: Parsec Void Text SqlPart
fragment = SyntaxFragment <$> takeWhile1P Nothing (not . isFragmentBoundary)

isFragmentBoundary :: Char -> Bool
isFragmentBoundary c = c == '?' || c == '\'' || c == '/' || c == '-'

comment :: Parsec Void Text SqlPart
comment = blockComment <|> lineComment

lineComment :: Parsec Void Text SqlPart
lineComment = do
  string "--"
  c <- takeWhileP Nothing (/= '\n')
  pure $ SyntaxFragment $ "--" <> c

blockComment :: Parsec Void Text SqlPart
blockComment = do
  string "/*"
  SyntaxFragment . Text.concat . ("/*" :) <$> chunks
  where
    chunks :: Parsec Void Text [Text]
    chunks = do
      curr <- takeWhileP Nothing (/= '*')
      rest <- ((: []) <$> string "*/") <|> ((:) <$> string "*" <*> chunks)
      pure (curr : rest)
