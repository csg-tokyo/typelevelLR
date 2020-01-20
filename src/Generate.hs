
module Generate where

import Utility       (pascalCase)
import Syntax        (Syntax(syntaxName))
import SyntaxParser  (parseSyntax, parse, eliminateComment)
import LALRAutomaton (lalrAutomaton)
import CodeGenerateEnv

import qualified GenerateHaskell      as GenHs
import qualified GenerateCpp          as GenCpp
import qualified GenerateTypeScript   as GenTypeScript
import qualified GenerateScala        as GenScala

import System.FilePath       ((</>))
import Data.Monoid           (Endo(appEndo))
import Control.Monad.Writer  (execWriter, Writer)
import Control.Monad.Reader  (ReaderT(runReaderT))

-------------------------------------------------------------------------------

generate :: Writer (Endo String) any -> String
generate = (`appEndo` "") . execWriter

-------------------------------------------------------------------------------

generateHaskell :: FilePath -> FilePath -> IO ()
generateHaskell src dst = do
  syntaxSource <- readFile src
  let syntax = case parse parseSyntax src (eliminateComment syntaxSource) of
        Left  err -> error (show err)
        Right s   -> s

  let haskellFilePath = dst </> (pascalCase (syntaxName syntax) ++ ".hs")

  let haskellCode = generate (GenHs.tellHaskell syntax)

  writeFile haskellFilePath haskellCode

-------------------------------------------------------------------------------

generateCpp :: FilePath -> FilePath -> IO ()
generateCpp src dst = do
  syntaxSource <- readFile src
  let syntax = case parse parseSyntax src (eliminateComment syntaxSource) of
        Left  err -> error (show err)
        Right s   -> s
  let automaton = lalrAutomaton syntax
  let env = buildCodeGenerateEnv syntax automaton

  let hppFilePath = dst </> (syntaxName syntax ++ ".hpp")
  let cppFilePath = dst </> (syntaxName syntax ++ ".cpp")
  let hppImplFilePath = dst </> (syntaxName syntax ++ ".hpp.impl")

  let hppCode     = generate (runReaderT GenCpp.tellHpp     env)
  let cppCode     = generate (runReaderT GenCpp.tellCpp     env)
  let hppImplCode = generate (runReaderT GenCpp.tellHppImpl env)

  writeFile  hppFilePath     hppCode
  writeFile  cppFilePath     cppCode
  writeFile  hppImplFilePath hppImplCode

-------------------------------------------------------------------------------

generateTypeScript :: FilePath -> FilePath -> IO ()
generateCpp src dst = do
  syntaxSource <- readFile src
  let syntax = case parse parseSyntax src (eliminateComment syntaxSource) of
        Left  err -> error (show err)
        Right s   -> s

  let tsFilePath = dst </> (pascalCase (syntaxName syntax) ++ ".ts")

  let tsCode = generate (GenTypeScript.tellTypeScript syntax)

  writeFile tsFilePath tsCode

-------------------------------------------------------------------------------

generateScala :: FilePath -> FilePath -> IO ()
generateScala src dst = do
  syntaxSource <- readFile src
  let syntax = case parse parseSyntax src (eliminateComment syntaxSource) of
        Left  err -> error (show err)
        Right s   -> s
  let automaton = lalrAutomaton syntax
  let env = buildCodeGenerateEnv syntax automaton

  let scalaFilePath = dst </> (syntaxName syntax ++ ".scala")

  let scalaCode = generate (runReaderT GenScala.tellScala env)

  writeFile scalaFilePath scalaCode

-------------------------------------------------------------------------------
