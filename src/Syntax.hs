
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Syntax (module Syntax) where

import Utility

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad         (forM_, mzero)
import Control.Arrow         (second)
import Control.Monad.Writer  (MonadWriter(), execWriter)
import Data.Monoid           ((<>), Endo(appEndo))
import Data.List             (nub, tails)

import Debug.Trace

-------------------------------------------------------------------------------

data Syntax = Syntax {
  syntaxName       :: String,
  syntaxStart      :: NonTerminal,
  syntaxRulesTable :: Map.Map NonTerminal [Rule]
}

data Rule = Rule { ruleName :: String,
                   ruleLhs  :: NonTerminal,
                   ruleRhs  :: [Symbol] }
  deriving (Eq, Ord)

data Symbol = NonTerminalSymbol NonTerminal
            | TerminalSymbol    Terminal
  deriving (Eq, Ord)

data NonTerminal = StartSymbol
                 | UserNonTerminal String
  deriving (Eq, Ord)

data Terminal = UserTerminal String [String]
              | EndOfInput
  deriving (Eq, Ord)

-------------------------------------------------------------------------------

startRule :: NonTerminal -> Rule
startRule nt = Rule "(START)" StartSymbol [NonTerminalSymbol nt]

syntax :: String -> NonTerminal -> [Rule] -> Syntax
syntax name start rules = Syntax name start ruleTable
  where ruleTable = Map.fromListWith (flip (++)) [(ruleLhs rule, [rule]) | rule <- rules]

syntaxRules :: Syntax -> NonTerminal -> [Rule]
syntaxRules syntax nt = Map.findWithDefault [] nt (syntaxRulesTable syntax)

syntaxNonTerminals :: Syntax -> [NonTerminal]
syntaxNonTerminals syntax = nub ([start] ++ lhss ++ rhss)
  where start = syntaxStart syntax
        lhss  = Map.keys (syntaxRulesTable syntax)
        rhss  = Map.elems (syntaxRulesTable syntax) >>= id >>= ruleRhs >>= \case
          NonTerminalSymbol nt -> return nt
          _                    -> mzero

syntaxTerminals :: Syntax -> [Terminal]
syntaxTerminals syntax = nub $ Map.elems (syntaxRulesTable syntax) >>= id >>= ruleRhs >>= \case
  TerminalSymbol t -> return t
  _                -> mzero


nonTerminalName :: NonTerminal -> String
nonTerminalName StartSymbol            = "(start)"
nonTerminalName (UserNonTerminal name) = name

terminalName :: Terminal -> String
terminalName (UserTerminal name _) = name
terminalName EndOfInput            = "end"

terminalParams :: Terminal -> [String]
terminalParams (UserTerminal _ params) = params
terminalParams EndOfInput              = []

-------------------------------------------------------------------------------

tellSyntaxSingleLine :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellSyntaxSingleLine syntax = do
  tells "syntax " >> tells (syntaxName syntax)
  tells " (" >> tellNonTerminal (syntaxStart syntax) >> tells ") { "
  forMWithSep_ (tells "; ") (syntaxNonTerminals syntax) $ \nt -> do
    mapMWithSep_ (tells "; ") tellRule (syntaxRules syntax nt)
  tells " }"

tellSyntaxMultiLine :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellSyntaxMultiLine syntax = do
  tells "syntax " >> tells (syntaxName syntax)
  tells " (" >> tellNonTerminal (syntaxStart syntax) >> tellsLn ") {"
  forM_ (syntaxNonTerminals syntax) $ \nt -> do
    forM_ (syntaxRules syntax nt) $ \rule -> do
      tells "  " >> tellRule rule >> tellNewline
  tells "}"

tellRule :: (MonadWriter (Endo String) m) => Rule -> m ()
tellRule rule = do
  tells (ruleName rule) >> tells " : "
  tellNonTerminal (ruleLhs rule) >> tells " -> "
  case ruleRhs rule of
    []  -> tells "eps"
    rhs -> mapMWithSep_ (tells " ") tellSymbol rhs

tellSymbol :: (MonadWriter (Endo String) m) => Symbol -> m ()
tellSymbol (NonTerminalSymbol nt) = tellNonTerminal nt
tellSymbol (TerminalSymbol    t)  = tellTerminal    t

tellNonTerminal :: (MonadWriter (Endo String) m) => NonTerminal -> m ()
tellNonTerminal nt = tells (nonTerminalName nt)

tellTerminal :: (MonadWriter (Endo String) m) => Terminal -> m ()
tellTerminal (UserTerminal name params) = do
  tells "\"" >> tells name >> tells "("
  case params of [] -> return ()
                 _  -> mapMWithSep_ (tells ", ") tells params
  tells ")\""
tellTerminal EndOfInput        = tells "$"

-------------------------------------------------------------------------------

instance Show Syntax where
  showsPrec 0 syntax = tolds (tellSyntaxMultiLine syntax)
  showsPrec d syntax = showParen (d > 10) (tolds (tellSyntaxSingleLine syntax))

instance Show Rule where
  showsPrec d rule = showString "Rule[" . tolds (tellRule rule) . showString "]"

instance Show Symbol where
  showsPrec d symbol = showParen (d > 10) $
                       showString "Symbol " . tolds (tellSymbol symbol)

instance Show NonTerminal where
  showsPrec d nt = showParen (d > 10) $
    showString "NonTerminal " . tolds (tellNonTerminal nt)

instance Show Terminal where
  showsPrec d t = showParen (d > 10) $
    showString "Terminal " . tolds (tellTerminal t)

-------------------------------------------------------------------------------

type NullableTable = Map.Map NonTerminal Bool

nonTerminalNullable :: NullableTable -> NonTerminal -> Bool
nonTerminalNullable table nt = table Map.! nt

buildNullableTable :: Syntax -> NullableTable
buildNullableTable s = fixPoint grow initialTable
  where initialTable = Map.fromList [(nt, False) | nt <- syntaxNonTerminals s]
        grow table = (`Map.mapWithKey` table) $ \nt b -> b ||
          any (all (symbolNullable table) . ruleRhs) (syntaxRules s nt)
        symbolNullable table symbol = case symbol of
          NonTerminalSymbol nt -> nonTerminalNullable table nt
          _                    -> False

-------------------------------------------------------------------------------

type FirstSetTable = Map.Map NonTerminal (Set.Set Terminal)

firstSetTableLookup :: FirstSetTable -> NonTerminal -> Set.Set Terminal
firstSetTableLookup table nt = Map.findWithDefault Set.empty nt table

firstSet :: Syntax -> NullableTable -> FirstSetTable -> Set.Set Terminal -> [Symbol] -> Set.Set Terminal
firstSet _      _        _     la []                            = la
firstSet syntax nullable table la (NonTerminalSymbol nt : rest)
  | nonTerminalNullable nullable nt = fs <> firstSet syntax nullable table la rest
  | otherwise                     = fs
  where fs = Map.findWithDefault Set.empty nt table
firstSet _      _        _     la (TerminalSymbol t : rest)     = Set.singleton t

-- nonTerminalNullable :: Syntax -> NonTerminal -> Bool
-- nonTerminalNullable syntax nt = any (all symbolNullable . ruleRhs) rules
--   where rules = syntaxRules syntax nt
--         symbolNullable (NonTerminalSymbol nt') = nonTerminalNullable syntax nt'
--         symbolNullable _                       = False

buildFirstSetTable :: Syntax -> NullableTable -> FirstSetTable
buildFirstSetTable syntax nullable = fixPoint grow initialTable
  where initialTable = Map.fromList [(nt, Set.empty) |
                                     nt <- syntaxNonTerminals syntax]
        grow table = Map.mapWithKey (growOne table) table
        growOne table nt la = Set.union la $
          foldMap (firstSet syntax nullable table Set.empty . ruleRhs) (syntaxRules syntax nt)

-------------------------------------------------------------------------------
