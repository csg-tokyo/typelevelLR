
{-# LANGUAGE FlexibleContexts #-}

module SyntaxParser (module SyntaxParser, parse) where

import Syntax
import Text.Parsec

import Data.Char  (isSpace)

-------------------------------------------------------------------------------

sep :: (Stream s m Char) => ParsecT s u m ()
sep = () <$ newline <|> () <$ string ";"

ws' :: (Stream s m Char) => ParsecT s u m ()
ws' = () <$ many space

ws :: (Stream s m Char) => ParsecT s u m ()
ws = () <$ many1 space

trySepBy :: (Stream s m c) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
trySepBy a sep = (:) <$> a <*> many (try (sep *> a))

-------------------------------------------------------------------------------

parseSyntax :: (Stream s m Char) => ParsecT s u m Syntax
parseSyntax = do
  name <- ws' *> string "syntax" *> ws *> parseIdentifier
  ws'
  start <- string "(" *> ws' *> parseNonTerminal <* ws' <* string ")"
  ws'
  rules <- string "{" *> ws' *> parseRuleDefs <* many (() <$ space <|> sep) <* string "}"
  return (syntax name start rules)
  

parseRuleDefs :: (Stream s m Char) => ParsecT s u m [(NonTerminal, Rule)]
parseRuleDefs = trySepBy parseRuleDef (manyTill space (try sep))

parseRuleDef :: (Stream s m Char) => ParsecT s u m (NonTerminal, Rule)
parseRuleDef = do
  many (() <$ space <|> sep)
  name <- parseIdentifier
  ws' *> string ":" *> ws'
  nt <- parseNonTerminal
  ws' *> (string "=" <|> string "->") *> ws'
  symbols <- parseDerivation
  return (nt, [(name, symbols)])

parseDerivation :: (Stream s m Char) => ParsecT s u m [Symbol]
parseDerivation  =  [] <$ string "eps"
                <|> trySepBy (ws' *> parseSymbol) (notFollowedBy (ws' *> parseIdentifier *> ws' *> (string ":")))

parseSymbol :: (Stream s m Char) => ParsecT s u m Symbol
parseSymbol  =  Right <$> try parseTerminal  -- id, int, eps are priored
            <|> Left  <$> parseNonTerminal
            -- <|> string "(" *> ws' *> parseBranch <* ws' <* string ")"

parseNonTerminal :: (Stream s m Char) => ParsecT s u m NonTerminal
parseNonTerminal = NonTerminal <$> parseIdentifier

parseTerminal :: (Stream s m Char) => ParsecT s u m Terminal
parseTerminal  =  try parseKeyword
              <|> try parseStringLiteral
              <|> parseIntLiteral

parseKeyword :: (Stream s m Char) => ParsecT s u m Terminal
parseKeyword = Keyword <$ string "\"" <*> parseIdentifier <* string "\""

parseStringLiteral :: (Stream s m Char) => ParsecT s u m Terminal
parseStringLiteral = StringLiteral <$ string "str"

parseIntLiteral :: (Stream s m Char) => ParsecT s u m Terminal
parseIntLiteral = IntLiteral <$ string "int"

parseIdentifier :: (Stream s m Char) => ParsecT s u m String
parseIdentifier = many1 (satisfy isIdentifierChar)
  where isIdentifierChar c = not (isSpace c) && not (c `elem` "\"()=-|,;:{}")
  -- 対象言語によって使える文字が異なるので，とりあえず受け入れておいてから対象言語のコンパイラにエラーを出してもらう方針
  -- '"', '(', ')' を identifier につかうなら try を追加する必要がある

-------------------------------------------------------------------------------
