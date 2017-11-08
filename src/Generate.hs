
module Generate where

import Utility       (pascalCase)
import Syntax        (Syntax(syntaxName))
import SyntaxParser  (parseSyntax, parse)
import LALRAutomaton (lalrAutomaton)

import qualified GenerateHaskell as GenHs
import qualified GenerateCpp     as GenCpp

import System.FilePath       ((</>))
import Data.Monoid           (Endo(appEndo))
import Control.Monad.Writer  (execWriter, Writer)

-------------------------------------------------------------------------------

generate :: Writer (Endo String) any -> String
generate = (`appEndo` "") . execWriter

-------------------------------------------------------------------------------

generateHaskell :: FilePath -> FilePath -> IO ()
generateHaskell src dst = do
  syntaxSource <- readFile src
  let syntax = case parse parseSyntax src syntaxSource of
        Left  err -> error (show err)
        Right s   -> s

  let haskellFilePath = dst </> (pascalCase (syntaxName syntax) ++ ".hs")

  let haskellCode = generate (GenHs.tellHaskell syntax)

  writeFile haskellFilePath haskellCode

-------------------------------------------------------------------------------

generateCpp :: FilePath -> FilePath -> IO ()
generateCpp src dst = do
  syntaxSource <- readFile src
  let syntax = case parse parseSyntax src syntaxSource of
        Left  err -> error (show err)
        Right s   -> s
  let automaton = lalrAutomaton syntax
  let nodeInfo  = GenCpp.buildNodeInfoTable automaton

  let hppFilePath = dst </> (syntaxName syntax ++ ".hpp")
  let cppFilePath = dst </> (syntaxName syntax ++ ".cpp")
  let hppImplFilePath = dst </> (syntaxName syntax ++ ".hpp.impl")

  let hppCode     = generate (GenCpp.tellHpp     syntax automaton nodeInfo)
  let cppCode     = generate (GenCpp.tellCpp     syntax automaton nodeInfo)
  let hppImplCode = generate (GenCpp.tellHppImpl syntax automaton nodeInfo)

  writeFile  hppFilePath     hppCode
  writeFile  cppFilePath     cppCode
  writeFile  hppImplFilePath hppImplCode

-------------------------------------------------------------------------------
