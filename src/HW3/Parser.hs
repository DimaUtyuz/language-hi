module HW3.Parser
  ( parse,
  )
where

import HW3.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))
import Data.Void (Void)
import Data.Word (Word8)
import Data.Text (pack)
import Data.List (intercalate)
import qualified Data.ByteString (pack)
import Data.Char (isAlpha, isAlphaNum)
import Text.Megaparsec (ParseErrorBundle, Parsec, between, try, choice, empty, eof, some, many, runParser, manyTill,
                        sepBy, sepBy1, notFollowedBy, satisfy, sepEndBy, (<?>))
import Text.Megaparsec.Char (char, hexDigitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)

-- the underlying mechanisms have been studied and taken from the articles
-- https://markkarpov.com/tutorial/megaparsec.html
-- https://serokell.io/blog/parser-combinators-in-haskell#megaparsec-tutorial

-- expression parser with skipping spaces at the beginning and parsing to the end of the string
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (between skipSpace eof parseExprWithOperators) ""

-- expression parser with binary operators and parenthesized arguments, run(!) and dot access syntax:
-- "expr * expr" | "expr(expr, expr, ...)" | "expr!" | "expr.hello"
-- for this, the operator table is used
parseExprWithOperators :: Parser HiExpr
parseExprWithOperators = makeExprParser (choice [parseExpr, parens parseExprWithOperators]) operators <?> "general expression"

-- parser of value, list or dictionary: "value" | "[expr, expr, ...]" | "{expr: expr, expr: expr, ...}"
parseExpr :: Parser HiExpr
parseExpr = choice [parseExprValue, parseList, parseDict] <?> "expression without operators"

-- parser of parenthesized function argument, run and dot access syntax: "(expr, expr, ...)" | "!" | ".hello"
parseUnaryOps :: Parser (HiExpr -> HiExpr)
parseUnaryOps = foldl1 (flip (.)) <$> some (choice [parseArgs, parseRun, parseDotFunction]) <?> "unary operation"

type Parser = Parsec Void String

-- skip 1 or more spaces, a line comment, and a block comment: "    " -> skip
skipSpace :: Parser ()
skipSpace = Lexer.space space1 empty empty

-- skip spaces after given parser "a   " -> a
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme skipSpace

-- make string parser and skip spaces: "{s}    " -> s
symbol :: String -> Parser String
symbol = Lexer.symbol skipSpace

-- string parser for the absence of another string immediately next: "{a}{c}" -> a, c /= b
parseNotFollowedBy :: String -> String -> Parser String
parseNotFollowedBy a b = (lexeme . try) $ string a <* notFollowedBy (string b)

-- parser wrapper with start and end string appended: "{left}a{right}" -> a
parseBetween :: String -> String -> Parser a -> Parser a
parseBetween left right = between (symbol left) (symbol right)

-- parsing inside parentheses: "(...)"
parens :: Parser a -> Parser a
parens = parseBetween "(" ")"

-- function name parser: "add" -> HiFunAdd
parseBuiltInName :: Parser HiValue
parseBuiltInName = HiValueFunction <$> choice
  [ HiFunAdd <$ symbol "add",
    HiFunSub <$ symbol "sub",
    HiFunMul <$ symbol "mul",
    HiFunDiv <$ symbol "div",
    HiFunNotLessThan <$ symbol "not-less-than",
    HiFunNotGreaterThan <$ symbol "not-greater-than",
    HiFunNotEquals <$ symbol "not-equals",
    -- it's better to add notFollowedBy even though this parser is listed behind the not-less-than parser and the like
    -- as they said in practice on programming paradigms: "What if someone moves your code? Call the next one"
    HiFunNot <$ parseNotFollowedBy "not" "-",
    HiFunAnd <$ symbol "and",
    HiFunOr <$ symbol "or",
    HiFunLessThan <$ symbol "less-than",
    HiFunGreaterThan <$ symbol "greater-than",
    HiFunEquals <$ symbol "equals",
    HiFunIf <$ symbol "if",
    HiFunLength <$ symbol "length",
    HiFunToUpper <$ symbol "to-upper",
    HiFunToLower <$ symbol "to-lower",
    HiFunReverse <$ symbol "reverse",
    HiFunTrim <$ symbol "trim",
    HiFunList <$ symbol "list",
    HiFunRange <$ symbol "range",
    HiFunFold <$ symbol "fold",
    HiFunPackBytes <$ symbol "pack-bytes",
    HiFunUnpackBytes <$ symbol "unpack-bytes",
    HiFunEncodeUtf8 <$ symbol "encode-utf8",
    HiFunDecodeUtf8 <$ symbol "decode-utf8",
    HiFunZip <$ symbol "zip",
    HiFunUnzip <$ symbol "unzip",
    HiFunSerialise <$ symbol "serialise",
    HiFunDeserialise <$ symbol "deserialise",
    HiFunRead <$ symbol "read",
    HiFunWrite <$ symbol "write",
    HiFunMkDir <$ symbol "mkdir",
    HiFunChDir <$ symbol "cd",
    HiFunParseTime <$ symbol "parse-time",
    HiFunRand <$ symbol "rand",
    HiFunEcho <$ symbol "echo",
    HiFunCount <$ symbol "count",
    HiFunKeys <$ symbol "keys",
    HiFunValues <$ symbol "values",
    HiFunInvert <$ symbol "invert"
  ] <?> "function name"

-- parser of number: "2" | "-8" | "12.3e5" | "3.5"
parseNumericLiteral :: Parser HiValue
parseNumericLiteral = lexeme (HiValueNumber . toRational <$> Lexer.signed skipSpace Lexer.scientific) <?> "number"

-- parser of bool:  "true" | "false"
parseBool :: Parser HiValue
parseBool = HiValueBool <$> choice [True <$ symbol "true", False <$ symbol "false"] <?> "bool"

-- parser of null value: "null"
parseNull :: Parser HiValue
parseNull = HiValueNull <$ symbol "null" <?> "null"

-- parser of string (chars inside quotes): "\"hello\"" -> "hello"
parseStringLiteral :: Parser HiValue
parseStringLiteral = lexeme (HiValueString . pack <$> (char '\"' *> manyTill Lexer.charLiteral (char '\"'))) <?> "string"

-- parser of byte (two hex digits): "0e" | "ff"
parseByte :: Parser Word8
parseByte = (\(a, b) -> read $ "0x" <> [a, b]) <$> ((,) <$> hexDigitChar <*> hexDigitChar) <?> "byte"

-- parser of byte list: "[# 00 3d ff #]"
parseBytes :: Parser HiValue
parseBytes = lexeme (HiValueBytes . Data.ByteString.pack <$> parseBetween "[#" "#]" (sepEndBy parseByte space1)) <?> "byte array"

-- parser of name of action without arguments: "cwd" | "now"
parseAction :: Parser HiValue
parseAction = HiValueAction <$> choice [HiActionCwd <$ symbol "cwd", HiActionNow <$ symbol "now"] <?> "action"

-- parser of values (function names, numbers, bools, null, strings, byte lists, actions):
-- "add" | "1e4" | "true" | "null" | "\"hello\"" | "[# 2e f3 #]" | "now"
parseExprValue :: Parser HiExpr
parseExprValue = HiExprValue <$> choice
  [ parseNull,
    parseAction,
    parseBool,
    parseBuiltInName,
    parseNumericLiteral,
    parseStringLiteral,
    parseBytes
  ]

-- parser of full expressions separated by commas: "expr, expr, ..."
parseElements :: (Parser [HiExpr] -> Parser [HiExpr]) -> Parser [HiExpr]
parseElements elementsParser = elementsParser $ sepBy parseExprWithOperators $ symbol ","

-- parser of parenthesized function arguments: "(expr, expr, ...)" -> (function name -> function)
parseArgs :: Parser (HiExpr -> HiExpr)
parseArgs = flip HiExprApply <$> parseElements parens <?> "arguments"

-- parser of expression list: "[expr, expr, ...]" -> list(expr, expr, ...)
parseList :: Parser HiExpr
parseList = HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> parseElements (parseBetween "[" "]") <?> "list"

-- parser of run syntax: "!" -> (action name -> action)
parseRun :: Parser (HiExpr -> HiExpr)
parseRun = HiExprRun <$ symbol "!" <?> "run"

-- parser of dictionary: "{expr1: expr2, expr3: expr4, ...}" -> HiExprDict [(expr1, expr2), (expr3, expr4), ...]
parseDict :: Parser HiExpr
parseDict = HiExprDict <$> parseBetween "{" "}" (sepBy parseEntry $ symbol ",") <?> "dict"
  where
    parseEntry :: Parser (HiExpr, HiExpr)
    parseEntry = (,) <$> (parseExprWithOperators <* symbol ":") <*> parseExprWithOperators

-- parser of dot access function: ".field" -> (expr -> expr("field"))
parseDotFunction :: Parser (HiExpr -> HiExpr)
parseDotFunction = (\field expr -> HiExprApply expr [joinFieldParts field]) <$> (symbol "." *> parseField) <?> "dot access"
  where
    parseField :: Parser [[Char]]
    parseField = lexeme $ ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'

    joinFieldParts :: [[Char]] -> HiExpr
    joinFieldParts field = HiExprValue . HiValueString . pack $ intercalate "-" field

-- table of operators
operators :: [[Operator Parser HiExpr]]
operators =
  [ [ Postfix parseUnaryOps ], -- max precedence
    [ makeInfixL "*" HiFunMul, -- precedence: 7
      makeBinOperator InfixL HiFunDiv (parseNotFollowedBy "/" "=")
    ],
    [ makeInfixL "+" HiFunAdd, -- precedence: 6
      makeInfixL "-" HiFunSub
    ],
    [ makeInfix ">=" HiFunNotLessThan, -- precedence: 4
      makeInfix "<=" HiFunNotGreaterThan,
      makeInfix "==" HiFunEquals,
      makeInfix "/=" HiFunNotEquals,
      -- it is better to add notFollowedBy although these operators are listed after <= and >=
      -- as they said in practice on programming paradigms: "What if someone moves your code? Call the next one"
      makeBinOperator InfixN HiFunLessThan (parseNotFollowedBy "<" "="),
      makeBinOperator InfixN HiFunGreaterThan (parseNotFollowedBy ">" "=")
    ],
    [ makeInfixR "&&" HiFunAnd -- precedence: 3
    ],
    [ makeInfixR "||" HiFunOr -- precedence: 2
    ]
  ]
  where
    -- make binary operator with left associativity
    makeInfixL :: String -> HiFun -> Operator Parser HiExpr
    makeInfixL op func = makeBinOperator InfixL func (symbol op)

    -- make binary operator without associativity
    makeInfix :: String -> HiFun -> Operator Parser HiExpr
    makeInfix op func = makeBinOperator InfixN func (symbol op)

    -- make binary operator with right associativity
    makeInfixR :: String -> HiFun -> Operator Parser HiExpr
    makeInfixR op func = makeBinOperator InfixR func (symbol op)

    -- make binary operator with given associativity
    makeBinOperator :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> HiFun -> Parser String -> Operator Parser HiExpr
    makeBinOperator assoc func parser = assoc ((\l r -> HiExprApply (HiExprValue $ HiValueFunction func) [l, r]) <$ parser)
