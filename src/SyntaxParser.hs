
{-# LANGUAGE FlexibleContexts #-}

module SyntaxParser (module SyntaxParser, parse) where

import Syntax
import Text.Parsec

import Data.Char  (isSpace)
import Data.List  (isPrefixOf)

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

eliminateComment :: String -> String
eliminateComment = unlines . go 0 . lines
  where go _ []       = []
        go d (x : xs) | "#{" `isPrefixOf` x = go (d + 1) xs
                      | "#}" `isPrefixOf` x = go (d - 1) xs
                      | d > 0              = go d xs
                      | "##" `isPrefixOf` x = go d xs
                      | otherwise           = x : go d xs

parseSyntax :: (Stream s m Char) => ParsecT s u m Syntax
parseSyntax = do
  name <- ws' *> string "syntax" *> ws *> parseIdentifier
  start <- ws' *> string "(" *> ws' *> parseNonTerminal <* ws' <* string ")"
  rules <- ws' *> string "{" *> ws' *> parseRules
  many (() <$ space <|> sep) <* string "}"
  return (syntax name start rules)

parseRules :: (Stream s m Char) => ParsecT s u m [Rule]
parseRules = trySepBy parseRule (manyTill space (try sep))

parseRule :: (Stream s m Char) => ParsecT s u m Rule
parseRule = do
  many (() <$ space <|> sep)
  name <- parseIdentifier
  ws' *> string ":" *> ws'
  lhs <- parseNonTerminal
  ws' *> (string "=" <|> string "->" <|> string "<-") *> ws'
  rhs <- parseDerivation
  return (Rule name lhs rhs)

parseDerivation :: (Stream s m Char) => ParsecT s u m [Symbol]
parseDerivation  = trySepBy (ws' *> parseSymbol) (notFollowedBy (ws' *> parseIdentifier *> ws' *> (string ":")))
                <|> [] <$ string "eps"

parseSymbol :: (Stream s m Char) => ParsecT s u m Symbol
parseSymbol  =  NonTerminalSymbol <$> try parseNonTerminal
            <|> TerminalSymbol    <$> parseTerminal
            -- <|> string "(" *> ws' *> parseBranch <* ws' <* string ")"

parseNonTerminal :: (Stream s m Char) => ParsecT s u m NonTerminal
parseNonTerminal = UserNonTerminal <$> parseIdentifier

parseTerminal :: (Stream s m Char) => ParsecT s u m Terminal
parseTerminal = do
  string "\""
  name <- parseIdentifier
  params <- option [] $ do
    string "(" *> ws'
    params <- option [] (trySepBy parseParam (ws' *> string "," *> ws'))
    ws' <* string ")"
    return params
  string "\""
  return (UserTerminal name params)

parseIdentifier :: (Stream s m Char) => ParsecT s u m String
parseIdentifier = many1 (satisfy isIdentifierChar)
  where isIdentifierChar c = not (isSpace c) && not (c `elem` "\"()=-|,;:{}")

parseParam :: (Stream s m Char) => ParsecT s u m String
parseParam = concat <$> many1 go
  where go  =  string "(" <++> parseParam <++> string ")"
           <|> string "{" <++> parseParam <++> string "}"
           <|> string "[" <++> parseParam <++> string "]"
           <|> pure <$> satisfy (\c -> not (c `elem` "()[]{}"))
        p1 <++> p2 = (++) <$> p1 <*> p2
  -- 対象言語によって使える文字が異なるので，とりあえず受け入れておいてから対象言語のコンパイラにエラーを出してもらう方針
  -- '"', '(', ')' を identifier につかうなら try を追加する必要がある

-------------------------------------------------------------------------------
