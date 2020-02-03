
{-# LANGUAGE FlexibleContexts #-}

module SwitchCase (fromRight, fromLeft, camelCase, pascalCase, snakeCase, allCaps, splitIdentifier) where

import Text.Parsec hiding (digit, lower, upper, many, many1)
import Data.Char
import Data.Either (isRight)
import Data.List   (intercalate)

-------------------------------------------------------------------------------

fromLeft :: (Show b) => Either a b -> a
fromLeft = either id (\b -> error ("fromLeft (" ++ show b ++ ")"))

fromRight :: (Show a) => Either a b -> b
fromRight = either (\a -> error ("fromRight(" ++ show a ++ ")")) id

-------------------------------------------------------------------------------

trySepBy :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
trySepBy a sep = trySepBy1 a sep <|> pure []

trySepBy1 :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
trySepBy1 a sep  =  try ((:) <$> a <* sep <*> trySepBy1 a sep)
                <|> try (pure <$> a)

-------------------------------------------------------------------------------

infixr 6 <++>
l <++> r = (++) <$> l <*> r

many :: (Stream s m c) => ParsecT s u m [a] -> ParsecT s u m [a]
many m = many1 m <|> pure []

many1 :: (Stream s m c) => ParsecT s u m [a] -> ParsecT s u m [a]
many1 m = (++) <$> try m <*> many m

cmpl :: (Stream s m c, Show c) => ParsecT s u m [c] -> ParsecT s u m [c]
cmpl p = pure <$ notFollowedBy p <*> anyToken

-------------------------------------------------------------------------------

sepChar :: (Stream s m Char) => ParsecT s u m String
sepChar = pure <$> (char '_' <|> char '-' <|> space)

digit :: (Stream s m Char) => ParsecT s u m String
digit = pure <$> satisfy isDigit

lower :: (Stream s m Char) => ParsecT s u m String
lower = pure <$> satisfy isLower

upper :: (Stream s m Char) => ParsecT s u m String
upper = pure <$> satisfy isUpper

alpha :: (Stream s m Char) => ParsecT s u m String
alpha = lower <|> upper

-------------------------------------------------------------------------------

sep :: (Stream s m Char) => ParsecT s u m [String]
sep = pure <$> many sepChar

number :: (Stream s m Char) => ParsecT s u m String
number = digit <++> many (cmpl (sepChar <|> alpha))

lowerWord :: (Stream s m Char) => ParsecT s u m String
lowerWord = lower <++> many (cmpl (sepChar <|> upper))

camelWord :: (Stream s m Char) => ParsecT s u m String
camelWord = upper <++> many (cmpl (sepChar <|> alpha)) <++> lower <++> many (cmpl (sepChar <|> upper))

upperWord :: (Stream s m Char) => ParsecT s u m String
upperWord = many1 (upper <++> many (cmpl (sepChar <|> alpha)) <* notFollowedBy lower)

unknownWord :: (Stream s m Char) => ParsecT s u m String
unknownWord = many1 (cmpl (sepChar <|> digit <|> alpha))

-------------------------------------------------------------------------------

isNumber :: String -> Bool
isNumber = isRight . parse number "isNumber"

isLowerWord :: String -> Bool
isLowerWord = isRight . parse lowerWord "isLowerWord"

isCamelWord :: String -> Bool
isCamelWord = isRight . parse camelWord "isCamelWord"

isUpperWord :: String -> Bool
isUpperWord = isRight . parse upperWord "isUpperWord"

isUnknownWord :: String -> Bool
isUnknownWord = isRight . parse unknownWord "isUnknownWord"

-------------------------------------------------------------------------------

word :: (Stream s m Char) => ParsecT s u m String
word = number <|> lowerWord <|> try camelWord <|> upperWord <|> unknownWord

splitIdentifier :: String -> [String]
splitIdentifier = fromRight . parse words "splitIdentifier"
  where words = sep <++> word `trySepBy` sep <++> sep

-------------------------------------------------------------------------------

shouldKeepUpperWord :: [String] -> Bool
shouldKeepUpperWord words = and [not (isUpperWord l) || not (isUpperWord r) |
                                 (l, r) <- zip words (tail words)]

splitWord :: String -> [String]
splitWord = fromRight . parse (many block) "splitWord"
  where block   = pure <$> (many1 alpha <|> many1 (cmpl alpha))

toCamel :: Bool -> String -> String
toCamel keepUpper word = splitWord word >>= \b -> case b of
  c : cs -> if keepUpper then toUpper c : cs else toUpper c : map toLower cs

-------------------------------------------------------------------------------

camelCase :: String -> String
camelCase identifier = case words of
  l : first : rest -> l ++ map toLower first ++ (rest >>= toCamel keepUpper)
  where words = splitIdentifier identifier
        keepUpper = shouldKeepUpperWord words

pascalCase :: String -> String
pascalCase identifier = words >>= toCamel keepUpper
  where words = splitIdentifier identifier
        keepUpper = shouldKeepUpperWord words

snakeCase :: String -> String
snakeCase = intercalate "_" . map (map toLower) . splitIdentifier

allCaps :: String -> String
allCaps = intercalate "_" . map (map toUpper) . splitIdentifier

-------------------------------------------------------------------------------
