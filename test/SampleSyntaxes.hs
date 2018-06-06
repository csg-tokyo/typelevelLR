
module SampleSyntaxes where

import Utility
import Syntax
import SyntaxParser
import Data.Monoid (Endo(appEndo))
import Control.Monad.Writer (MonadWriter(), execWriter)

-------------------------------------------------------------------------------

sampleSyntaxSource1 :: String
sampleSyntaxSource1 = "syntax sampleSyntax1 ( S ) { fork : S -> A A ; genA : A -> \"a\" A ; genB : A -> \"b\" }"

sampleSyntaxSource2 :: String
sampleSyntaxSource2 = "syntax sampleSyntax2(A){a:A->\"a\"A;fromB:A->B;ab:B->\"a\"B\"b\";fin:B->eps}"

sampleSyntaxSource3 :: String
sampleSyntaxSource3 = "syntax sampleSyntax3 (E) {; Add: E -> E \"add\" T; TToE: E -> T; Mul: T -> T \"mul\" F; FToT: T -> F; Num: F -> \"num( Int )\"; Paren : F -> \"lp\" E \"rp\" ;}"

sampleSyntaxSource4 :: String
sampleSyntaxSource4 = (`appEndo` "") $ execWriter $ do
  tellsLn "syntax sampleSyntax4 (Q1) {"
  tellsLn "  rule11 : Q1 -> \"a\" Q1"
  tellsLn "  rule12 : Q1 -> Q2"
  tellsLn "  rule21 : Q2 -> \"a\" Q2 \"d\""
  tellsLn "  rule22 : Q2 -> Q3"
  tellsLn "  rule31 : Q3 -> \"b\" Q3"
  tellsLn "  rule32 : Q3 -> Q4"
  tellsLn "  rule41 : Q4 -> \"b\" Q4 \"c\""
  tellsLn "  rule42 : Q4 -> eps"
  tellsLn "}"

-------------------------------------------------------------------------------

sampleSyntaxSources :: [String]
sampleSyntaxSources = [sampleSyntaxSource1,
                       sampleSyntaxSource2,
                       sampleSyntaxSource3,
                       sampleSyntaxSource4]

-------------------------------------------------------------------------------

sampleSyntax1 :: Syntax
sampleSyntax1 = fromRight (parse parseSyntax "sampleSyntax1" sampleSyntaxSource1)

sampleSyntax2 :: Syntax
sampleSyntax2 = fromRight (parse parseSyntax "sampleSyntax2" sampleSyntaxSource2)

sampleSyntax3 :: Syntax
sampleSyntax3 = fromRight (parse parseSyntax "sampleSyntax3" sampleSyntaxSource3)

sampleSyntax4 :: Syntax
sampleSyntax4 = fromRight (parse parseSyntax "sampleSyntax4" sampleSyntaxSource4)

-------------------------------------------------------------------------------

sampleSyntaxes :: [Syntax]
sampleSyntaxes = [sampleSyntax1,
                  sampleSyntax2,
                  sampleSyntax3,
                  sampleSyntax4]

-------------------------------------------------------------------------------
