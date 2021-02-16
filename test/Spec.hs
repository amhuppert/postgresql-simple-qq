{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import Database.PostgreSQL.Simple.TH
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testGroup
        "psqlStatement parser"
        [ unit_parsesSqlWithNoPlaceholders,
          unit_parsesSqlWithPlaceholders1,
          unit_parsesSqlWithPlaceholders2,
          unit_singleQuotes,
          unit_singleQuotesEscape,
          unit_blockComment,
          unit_lineComment
        ]
    ]

unit_parsesSqlWithNoPlaceholders :: TestTree
unit_parsesSqlWithNoPlaceholders = testCase "parses SQL with no placeholders" $ do
  let result = parseMaybe psqlStatement sql
  result @?= Just [SyntaxFragment sql]
  where
    sql = "SELECT * FROM users"

unit_parsesSqlWithPlaceholders1 :: TestTree
unit_parsesSqlWithPlaceholders1 = testCase "parses SQL with placeholders 1" $ do
  let result = parseMaybe psqlStatement sql
  result @?= Just [SyntaxFragment "SELECT * FROM users WHERE user_id = ", Placeholder "user_id"]
  where
    sql = "SELECT * FROM users WHERE user_id = ?user_id"

unit_parsesSqlWithPlaceholders2 :: TestTree
unit_parsesSqlWithPlaceholders2 = testCase "parses SQL with placeholders 2" $ do
  let result = parseMaybe psqlStatement sql
  result
    @?= Just
      [ SyntaxFragment f1,
        Placeholder "user_id",
        SyntaxFragment f2,
        Placeholder "someCol"
      ]
  where
    sql = f1 <> "?user_id" <> f2 <> "?someCol"
    f1 =
      Text.unlines
        [ "SELECT * ",
          "FROM users u ",
          "  INNER JOIN customer cust",
          "  ON u.customer_id = cust.customer_id",
          "WHERE user_id = "
        ]
    f2 = Text.unlines ["", "  AND cust.some_col = "]

unit_singleQuotes :: TestTree
unit_singleQuotes = testCase "? in single quote" $ do
  let result = parseMaybe psqlStatement sql
  result
    @?= Just
      [ SyntaxFragment "SELECT * FROM users WHERE x = ",
        SyntaxFragment "'abc ?x def'"
      ]
  where
    sql = "SELECT * FROM users WHERE x = 'abc ?x def'"

unit_singleQuotesEscape :: TestTree
unit_singleQuotesEscape = testCase "? in single quote with escaped single quote" $ do
  let result = parseMaybe psqlStatement sql
  result
    @?= Just
      [ SyntaxFragment "SELECT * FROM users WHERE x = ",
        SyntaxFragment "'abc\\' ?x def'"
      ]
  where
    sql = "SELECT * FROM users WHERE x = 'abc\\' ?x def'"

unit_blockComment :: TestTree
unit_blockComment = testCase "? in block comment" $ do
  let result = parseMaybe psqlStatement sql
  result
    @?= Just
      [ SyntaxFragment "SELECT * FROM ",
        SyntaxFragment "/* ?name */",
        SyntaxFragment " users"
      ]
  where
    sql = "SELECT * FROM /* ?name */ users"

unit_lineComment :: TestTree
unit_lineComment = testCase "? in line comment" $ do
  let result = parseMaybe psqlStatement sql
  result
    @?= Just
      [ SyntaxFragment f1,
        SyntaxFragment "-- ?name",
        SyntaxFragment "\nFROM users"
      ]
  where
    sql = f1 <> f2 <> "\n" <> f3
    f1 = "SELECT  * "
    f2 = "-- ?name"
    f3 = "FROM users"
