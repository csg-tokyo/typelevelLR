
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

main_ :: Flag "" '["hs", "haskell"] "" "generate haskell chain" Bool ->
         Flag "" '["cpp"]           "" "generate C++ chain"     Bool ->
         Flag "" '["scala"]         "" "generate Scala chain"   Bool ->
         Flag "n" '[] "N" "chain length" Int ->
         Flag "o" '["out-file"] "" "file to output" (Maybe String) ->
         Arg "FILENAME" String ->
         Cmd "generate random method chain" ()
main_ genHaskell_ genCpp_ genScala_ n_ out_ path_ = liftIO $ do
  let genHaskell = get genHaskell_
  let genCpp     = get genCpp_
  let genScala   = get genScala_
  let numAction  = length [() | b <- [genHaskell, genCpp, genScala], b]

  when (numAction <= 0) $ do
    putStrLn "none of --haskell, --cpp nor --scala is given"
    exitFailure

  when (numAction > 1) $ do
    putStrLn "multiple of --haskell, --cpp or --scala is given"

  let n = get n_
  let path = get path_

  src <- readFile path
  syntax <- case SyntaxParser.parse SyntaxParser.parseSyntax path (SyntaxParser.eliminateComment src) of
              Left err -> print err >> exitFailure
              Right s -> return s
  chain <- Gen.genRandomChain syntax (\start -> Gen.gen start n)

  let modName = if
        | genHaskell -> case get out_ of
            Nothing       -> "Main"
            Just filename | takeExtension filename == ".hs" -> dropExtension filename
                          | otherwise                       -> filename
        | genCpp -> case get out_ of
            Nothing       -> "main"
            Just filename | takeExtension filename == ".cpp" -> dropExtension filename
                          | otherwise                        -> filename
        | genScala -> case get out_ of
            Nothing -> "main.scala"
            Just filename | takeExtension filename == ".scala" -> dropExtension filename
                          | otherwise                          -> filename

  let outFile = if
        | genHaskell -> modName <.> "hs"
        | genCpp     -> modName <.> "cpp"
        | genScala   -> modName <.> "scala"

  let libName = if
        | genHaskell -> pascalCase (Syntax.syntaxName syntax)
        | genCpp     -> Syntax.syntaxName syntax
        | genScala   -> Syntax.syntaxName syntax

  writeFile outFile $ told $ if
    | genHaskell -> TellChain.tellHaskellSource modName libName chain
    | genCpp     -> TellChain.tellCppSource     modName libName chain
    | genScala   -> TellChain.tellScalaSource   modName libName chain

main :: IO ()
main = run_ main_

-------------------------------------------------------------------------------
