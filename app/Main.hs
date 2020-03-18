
{-# LANGUAGE DataKinds #-}

module Main where

import Generate  (generateHaskell, generateCpp, generateTypeScript, generateScala)

import Options.Declarative
import System.FilePath         ((</>), (<.>))
import System.Directory        (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit             (exitFailure)
import Control.Monad           (when, forM_)
import Control.Monad.IO.Class  (liftIO)
import Data.List               (isSuffixOf)

-------------------------------------------------------------------------------

main_ :: Flag "" '["hs", "haskell"]  "" "generate Haskell library" Bool ->
         Flag "" '["cpp"] "" "generate C++ library"     Bool ->
         Flag "" '["ts", "typescript"] "" "generate TypeScript library"     Bool ->
         Flag "" '["scala"] "" "generate Scala library" Bool ->
         Flag "d" '["dest"] "PATH" "destination directory path" (Maybe FilePath) ->
         Flag "s" '["source"] "PATH" "source directory path" (Def "." FilePath) ->
         Cmd "Generate a Fluent API library skeleton from an LR Grammar" ()
main_ genHs_ genCpp_ genTypeScript_ genScala_ dest_ source_ = liftIO $ do
  let genHs           = if get genHs_           then [generateHaskell     ] else []
  let genCpp          = if get genCpp_          then [generateCpp         ] else []
  let genTypeScript   = if get genTypeScript_   then [generateTypeScript  ] else []
  let genScala        = if get genScala_        then [generateScala       ] else []
  let actions         = genHs ++ genCpp ++ genTypeScript ++ genScala
  (dst, sources) <- expandSource (get dest_) (get source_)

  when (null actions) $ do
    putStrLn "nothing to do: neither --hs nor --cpp nor --scala nor --ts is given"
    exitFailure

  forM_ ((,) <$> actions <*> sources) $ \(act, src) -> do
    act src dst

-- TODO: make source optional
--       if no source is given, copmile all .syntax file in current working directory
-- TODO: complement ".syntax" automatically
-- TODO: if a directory is geven as source, compile all .syntax file in the directory

expandSource :: Maybe FilePath -> FilePath -> IO (FilePath, [FilePath])
expandSource dest source = do
  isDir        <- doesDirectoryExist source
  isFile       <- doesFileExist source
  isSyntaxFile <- doesFileExist (source <.> "syntax")
  case () of
    _ | isDir -> do
          sources <- filter (isSuffixOf ".syntax") <$> listDirectory source
          return (maybe source id dest, sources)
      | not isFile && isSyntaxFile -> do
          return (maybe "." id dest, [source <.> "syntax"])
      | otherwise -> do
          return (maybe "." id dest, [source])

-------------------------------------------------------------------------------

main :: IO ()
main = run_ main_

-------------------------------------------------------------------------------
