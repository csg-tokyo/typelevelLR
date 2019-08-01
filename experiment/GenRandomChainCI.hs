
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

import qualified GenRandomChain as Gen
import qualified Syntax as Syntax
import qualified SyntaxParser as SyntaxParser
import Options.Declarative
import System.Exit (exitFailure)
import Utility
import Control.Monad.Writer
import System.FilePath
import qualified TellChain as TellChain

-------------------------------------------------------------------------------

generateHaskell :: Flag "n" '[] "N" "chain length" Int ->
                   Flag "o" '[] "" "file to output" (Def "Main.hs" String) ->
                   Arg "FILENAME" String ->
                   Cmd "generate random method chain in Haskell" ()
generateHaskell n_ out_ path_ = liftIO $ do
  let n    = get n_
  let out  = get out_
  let path = get path_

  src <- readFile path
  let src' = SyntaxParser.eliminateComment src
  syntax <- case SyntaxParser.parse SyntaxParser.parseSyntax path src' of
    Left  err -> print err >> exitFailure
    Right s   -> return s
  chain <- Gen.genRandomChain syntax (\start -> Gen.gen start n)
  let modName = dropExtension out
  let libName = pascalCase (Syntax.syntaxName syntax)
  writeFile (modName <.> "hs") $ told $ do
    TellChain.tellHaskellSource modName libName chain


generateCpp :: Flag "n" '[] "N" "chain length" Int ->
               Flag "o" '[] "" "file to output" (Def "main.cpp" String) ->
               Arg "FILENAME" String ->
               Cmd "generate random method chain in C++" ()
generateCpp n_ out_ path_ = liftIO $ do
  let n    = get n_
  let out  = get out_
  let path = get path_

  src <- readFile path
  let src' = SyntaxParser.eliminateComment src
  syntax <- case SyntaxParser.parse SyntaxParser.parseSyntax path src' of
    Left  err -> print err >> exitFailure
    Right s   -> return s
  chain <- Gen.genRandomChain syntax (\start -> Gen.gen start n)
  let modName = dropExtension out
  let libName = Syntax.syntaxName syntax
  writeFile (modName <.> "cpp") $ told $ do
    TellChain.tellCppSource modName libName chain


generateScala :: Flag "n" '[] "N" "chain length" Int ->
                 Flag "o" '[] "" "file to output" (Def "main.scala" String) ->
                 Arg "FILENAME" String ->
                 Cmd "generate random method chain in Scala" ()
generateScala n_ out_ path_ = liftIO $ do
  let n    = get n_
  let out  = get out_
  let path = get path_

  src <- readFile path
  let src' = SyntaxParser.eliminateComment src
  syntax <- case SyntaxParser.parse SyntaxParser.parseSyntax path src' of
    Left  err -> print err >> exitFailure
    Right s   -> return s
  chain <- Gen.genRandomChain syntax (\start -> Gen.gen start n)
  let modName = dropExtension out
  let libName = Syntax.syntaxName syntax
  writeFile (modName <.> "scala") $ told $ do
    TellChain.tellScalaSource modName libName chain

-------------------------------------------------------------------------------

showHaskellLibName :: Arg "FILENAME" String ->
                      Cmd "display library name (Haskell version)" ()
showHaskellLibName path_ = liftIO $ do
  let path = get path_
  src <- readFile path
  let src' = SyntaxParser.eliminateComment src
  syntax <- case SyntaxParser.parse SyntaxParser.parseSyntax path src' of
    Left  err -> print err >> exitFailure
    Right s   -> return s
  putStrLn (pascalCase (Syntax.syntaxName syntax))


showCppLibName :: Arg "FILENAME" String ->
                  Cmd "display library name (C++ version)" ()
showCppLibName path_ = liftIO $ do
  let path = get path_
  src <- readFile path
  let src' = SyntaxParser.eliminateComment src
  syntax <- case SyntaxParser.parse SyntaxParser.parseSyntax path src' of
    Left  err -> print err >> exitFailure
    Right s   -> return s
  putStrLn (Syntax.syntaxName syntax)


showScalaLibName :: Arg "FILENAME" String ->
                    Cmd "display library name (Scala version)" ()
showScalaLibName path_ = liftIO $ do
  let path = get path_
  src <- readFile path
  let src' = SyntaxParser.eliminateComment src
  syntax <- case SyntaxParser.parse SyntaxParser.parseSyntax path src' of
    Left  err -> print err >> exitFailure
    Right s   -> return s
  putStrLn (Syntax.syntaxName syntax)

-------------------------------------------------------------------------------

main :: IO ()
main = run_ $ Group "experiment helper for typelevelLR"
  [subCmd "gen-haskell-chain"    generateHaskell,
   subCmd "gen-cpp-chain"        generateCpp,
   subCmd "gen-scala-chain"      generateScala,
   subCmd "show-haskell-libname" showHaskellLibName,
   subCmd "show-cpp-libname"     showCppLibName,
   subCmd "show-scala-libname"   showScalaLibName]

-------------------------------------------------------------------------------
