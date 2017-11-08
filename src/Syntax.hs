
{-# LANGUAGE FlexibleContexts #-}

module Syntax (module Syntax) where

import Utility

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Monoid           ((<>), Endo(appEndo))
import Control.Arrow         (second)
import Control.Monad.Writer  (MonadWriter(), execWriter)
import Data.Either           (lefts, rights)
import Data.List             (tails)

-------------------------------------------------------------------------------

data Syntax = Syntax {
  syntaxName      :: String,
  syntaxStart     :: NonTerminal,
  syntaxRuleTable :: Map.Map NonTerminal Rule
}

syntaxRules :: Syntax -> NonTerminal -> Rule
syntaxRules syntax nt = Map.findWithDefault [] nt (syntaxRuleTable syntax)

type Rule = [(String, [Symbol])]

type Symbol = Either NonTerminal Terminal

data NonTerminal = StartSymbol
                 | NonTerminal String
  deriving (Show, Eq, Ord)

nonTerminalName :: NonTerminal -> String
nonTerminalName StartSymbol = "(start)"
nonTerminalName (NonTerminal name) = name

data Terminal = Keyword String
              | StringLiteral
              | IntLiteral
              | EndOfInput
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------

syntax :: String -> NonTerminal -> [(NonTerminal, Rule)] -> Syntax
syntax name start rules = Syntax name start ruleTable
  where ruleTable = Map.fromListWith (flip (++)) rules

nonTerminals :: Syntax -> Set.Set NonTerminal
nonTerminals (Syntax _ start table) = Set.unions $
  [Set.singleton start,
   Map.keysSet table,
   Set.fromList (Map.elems table >>= map snd >>= lefts)]

terminals :: Syntax -> Set.Set Terminal
terminals (Syntax _ _ table) = Set.fromList (Map.elems table >>= map snd >>= rights)

-------------------------------------------------------------------------------

tellSyntax :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellSyntax syntax@(Syntax name start rules) = do
  tells "syntax " >> tells name
  tells " (" >> tells (nonTerminalName start) >> tells ") { "
  forMWithSep_ (tells "; ") (Set.toList (nonTerminals syntax)) $ \nt -> do
    tellDefinition syntax nt
  tells " }"

tellDefinition :: (MonadWriter (Endo String) m) => Syntax -> NonTerminal -> m ()
tellDefinition syntax nt = do
  forMWithSep_ (tells "; ") (syntaxRules syntax nt) $ \(name, expr) -> do
    tells (name ++ " : " ++ nonTerminalName nt ++ " -> ")
    tellExpr expr

tellExpr :: (MonadWriter (Endo String) m) => [Symbol] -> m ()
tellExpr []      = tells "eps"
tellExpr symbols = do
  forMWithSep_ (tells " ") symbols $ \symbol -> do
    tellSymbol symbol

tellSymbol :: (MonadWriter (Endo String) m) => Symbol -> m ()
tellSymbol = either tellNonTerminal tellTerminal

tellNonTerminal :: (MonadWriter (Endo String) m) => NonTerminal -> m ()
tellNonTerminal nt = tells (nonTerminalName nt)

tellTerminal :: (MonadWriter (Endo String) m) => Terminal -> m ()
tellTerminal (Keyword keyword) = tells "\"" >> tells keyword >> tells "\""
tellTerminal StringLiteral     = tells "str"
tellTerminal IntLiteral        = tells "int"
tellTerminal EndOfInput        = tells "<EndOfInput>"

instance Show Syntax where
  showsPrec d syntax = showParen (d > 0) $
    appEndo (execWriter (tellSyntax syntax))

-------------------------------------------------------------------------------

type FirstSetTable = Map.Map NonTerminal (Set.Set Terminal)

nullable :: Syntax -> NonTerminal -> Bool
nullable syntax nt = any (all (either (nullable syntax) (const False)) . snd) rules
  where rules = syntaxRules syntax nt

firstSetTable :: Syntax -> FirstSetTable
firstSetTable syntax = fixPoint grow initialTable
  where initialTable = Map.fromSet (const Set.empty) (nonTerminals syntax)
        grow table = flip Map.mapWithKey table $ \nt set ->
          set <> foldMap (firstSet syntax table Set.empty . snd) (syntaxRules syntax nt)

firstSet :: Syntax -> FirstSetTable -> Set.Set Terminal -> [Symbol] -> Set.Set Terminal
firstSet _      _     la []               = la
firstSet syntax table la (Left nt : rest)
  | nullable syntax nt = fs <> firstSet syntax table la rest
  | otherwise          = fs
  where fs = Map.findWithDefault Set.empty nt table
firstSet _      _     la (Right t : rest) = Set.singleton t

-------------------------------------------------------------------------------
