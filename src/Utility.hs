
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Utility (module Utility) where

import Control.Monad         (forM, forM_)
import Control.Monad.Writer  (MonadWriter(tell), Writer, execWriter)
import Control.Monad.State   (get, put, evalStateT)
import Control.Monad.Trans   (lift)
import Data.Monoid           (Endo(Endo, appEndo))
import Data.List             (intercalate, tails)
import Data.Char             (toUpper, toLower, isAlpha)
import Text.Parsec

import qualified Data.Map as Map
import qualified Data.Set as Set

-------------------------------------------------------------------------------

for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap

orElse :: Maybe a -> a -> a
orElse Nothing  a = a
orElse (Just a) _ = a

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

allPairs :: [a] -> [(a, a)]
allPairs xs = [(x1, x2) | x1:xs' <- tails xs, x2 <- xs']

fromLeft :: Either a b -> a
fromLeft = either id (error "fromLeft")

fromRight :: Either a b -> b
fromRight = either (error "fromRight") id

-------------------------------------------------------------------------------

equalingBy :: (b -> b -> Bool) -> (a -> b) -> a -> a -> Bool
equalingBy eq key = \a1 a2 -> eq (key a1) (key a2)

equaling :: (Eq b) => (a -> b) -> a -> a -> Bool
equaling = equalingBy (==)

-------------------------------------------------------------------------------

fixPointBy :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixPointBy eq f a = let a' = f a in
  if eq a' a then a else fixPointBy eq f a'

fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint = fixPointBy (==)

-------------------------------------------------------------------------------

told :: Writer (Endo String) any -> String
told m = tolds m ""

tolds :: Writer (Endo String) any -> ShowS
tolds = appEndo . execWriter


tellNewline :: (MonadWriter (Endo String) m) => m ()
tellNewline = tells "\n"

tells :: (MonadWriter (Endo [a]) m) => [a] -> m ()
tells xs = tell (Endo (xs ++))

tellsLn :: (MonadWriter (Endo String) m) => String -> m ()
tellsLn xs = tells xs >> tellNewline

tellsShow :: (MonadWriter (Endo String) m, Show a) => a -> m ()
tellsShow a = tell (Endo (shows a))

tellsShowLn :: (MonadWriter (Endo String) m, Show a) => a -> m ()
tellsShowLn a = tellsShow a >> tellNewline

-------------------------------------------------------------------------------

mapMWithSep :: (Monad m, Traversable t) => m sep -> (a -> m b) -> t a -> m (t b)
mapMWithSep sep f xs = (`evalStateT` False) $ forM xs $ \x -> do
  flag <- get
  put True
  if flag then lift (sep >> f x) else lift (f x)

mapMWithSep_ :: (Monad m, Foldable t) => m sep -> (a -> m b) -> t a -> m ()
mapMWithSep_ sep f xs = (`evalStateT` False) $ forM_ xs $ \x -> do
  flag <- get
  put True
  if flag then lift (sep >> f x) else lift (f x)

forMWithSep :: (Monad m, Traversable t) => m sep -> t a -> (a -> m b) -> m (t b)
forMWithSep sep = flip (mapMWithSep sep)

forMWithSep_ :: (Monad m, Foldable t) => m sep -> t a -> (a -> m b) -> m ()
forMWithSep_ sep = flip (mapMWithSep_ sep)

sequenceWithSep :: (Monad m, Traversable t) => m sep -> t (m a) -> m (t a)
sequenceWithSep sep = mapMWithSep sep id

sequenceWithSep_ :: (Monad m, Foldable t) => m sep -> t (m a) -> m ()
sequenceWithSep_ sep = mapMWithSep_ sep id

-------------------------------------------------------------------------------

splitIdentifier :: String -> [String]
splitIdentifier = fromRight . parse (sepBy word sep) "splitIdentifier"
  where word  =  (:) <$> lower <*> many (lower <|> digit)
             <|> try ((:) <$> (toLower <$> upper) <*> many1 (lower <|> digit))
             <|> (:) <$> upper <*> many (try (upper <* notFollowedBy lower) <|> digit)
             <|> many1 (satisfy (\c -> not (isAlpha c) && c /= '-' && c /= '_'))
        sep  =  () <$ many1 (string "-")
            <|> () <$ many1 (string "_")
            <|> return ()

camelCase :: String -> String
camelCase identifier = case splitIdentifier identifier of
  []           -> []
  first : rest -> map toLower first ++ (rest >>= camelWord)

pascalCase :: String -> String
pascalCase identifier = splitIdentifier identifier >>= camelWord

camelWord :: String -> String
camelWord []     = []
camelWord (c:cs) = toUpper c : cs

snakeCase :: String -> String
snakeCase = intercalate "_" . splitIdentifier

allCaps :: String -> String
allCaps = intercalate "_" . map (map toUpper) . splitIdentifier

-------------------------------------------------------------------------------

assertEq :: (Eq a, Show a) => a -> a -> a
assertEq a1 a2 = if a1 == a2 then a1 else error . told $ do
  tells "assertEq -- "
  tellsShow a1
  tells " /= "
  tellsShow a2

-------------------------------------------------------------------------------
