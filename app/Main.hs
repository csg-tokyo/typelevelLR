
{-# LANGUAGE DataKinds #-}

module Main where

import Generate  (generateHaskell, generateCpp)

import Options.Declarative
import System.FilePath         ((</>))
import Control.Monad           (when)
import Control.Monad.IO.Class  (liftIO)

-------------------------------------------------------------------------------

main_ :: Flag "" '["hs"]  "" "generate Haskell library" Bool ->
         Flag "" '["cpp"] "" "generate C++ library"     Bool ->
         Flag "d" '[] "PATH" "destination directory path" (Def "." String) ->
         Arg "FILENAME" String ->
         Cmd "Generate Fluent API Library from LR Grammar" ()
main_ genHs_ genCpp_ dest_ source_ = liftIO $ do
  let (genHs, genCpp, dest, source) = (get genHs_, get genCpp_, get dest_, get source_)
  when genHs  (generateHaskell source dest)
  when genCpp (generateCpp     source dest)
  when (not genHs && not genCpp) $ do
    putStrLn "nothing to do: neither --hs nor --cpp are given"

-------------------------------------------------------------------------------

main :: IO ()
main = run_ main_

-------------------------------------------------------------------------------
