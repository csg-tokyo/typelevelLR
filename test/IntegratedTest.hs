
module IntegratedTest where

import Test.Hspec
import System.Directory
import System.FilePath
import System.Process
import GHC.IO.Exception        (ExitCode(ExitSuccess, ExitFailure))
import Data.List               (isPrefixOf, isSuffixOf)
import Control.Monad           (filterM, when, unless, forM_, forM, (>=>))
import Control.Monad.Cont      (callCC, runContT)
import Control.Monad.Writer    (MonadWriter(tell), execWriterT)
import Control.Monad.Trans     (lift)
import Control.Monad.IO.Class  (MonadIO(liftIO))
import Data.Char               (isSpace, isAlpha, toLower)
import Generate                (generateHaskell, generateCpp)

-------------------------------------------------------------------------------

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond body = cond >>= \cond_ -> when cond_ body

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM cond body = cond >>= \cond_ -> unless cond_ body

dropEnd :: Int -> [a] -> [a]
dropEnd n = reverse . drop n . reverse

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-------------------------------------------------------------------------------

runShell :: (MonadIO m) => [String] -> m (ExitCode, String, String)
runShell []               = error "empty command list"
runShell (command : args) = liftIO $ do
  (exitCode, output, err) <- readProcessWithExitCode command args ""

  -- -- for debugging
  -- putStrLn (command ++ (args >>= (' ' :)))
  -- putStr output
  -- putStr err

  return (exitCode, output, err)

-------------------------------------------------------------------------------

getTestType :: String -> String
getTestType = map toLower
              . takeWhile isAlpha
              . dropWhile (`elem` "-_")
              . drop (length "test")

-------------------------------------------------------------------------------

isValidWorkspace :: FilePath -> FilePath -> IO Bool
isValidWorkspace base path = (`runContT` return) $ callCC $ \exit -> do
  unlessM (lift (doesDirectoryExist (base </> path))) $ do
    exit False
  unlessM (lift (doesFileExist (base </> path </> path <.> "syntax"))) $ do
    exit False
  return True

getWorkspaces :: IO [(FilePath, FilePath)]
getWorkspaces = do
  let basePath = "examples"
  paths      <- listDirectory basePath
  validPaths <- filterM (isValidWorkspace basePath) paths
  return [(basePath </> validPath, validPath) | validPath <- validPaths]

-------------------------------------------------------------------------------

-- shouldSetup :: FilePath -> IO Bool
-- shouldSetup syntaxName = do
--   syntaxModificated <- getModificationTime (syntaxName <.> "syntax")
--   haskellGenerated  <- getModificationTime (syntaxName <.> "hs")
--   hppGenerated      <- getModificationTime (syntaxName <.> "hpp")
--   cppGenerated      <- getModificationTime (syntaxName <.> "cpp")
--   hppImplGenerated  <- getModificationTime (syntaxName <.> "hpp.impl")
--   return (any (< syntaxModificated) [haskellGenerated,
--                                      hppGenerated,
--                                      cppGenerated,
--                                      hppImplGenerated])

setupWorkspace :: FilePath -> FilePath -> IO ()
setupWorkspace workspace syntaxName = withCurrentDirectory workspace $ do
  -- whenM (shouldSetup syntaxName) $ do
    unlessM (doesFileExist (syntaxName <.> "syntax")) $ do
      error "no syntax file found"
    generateHaskell (syntaxName <.> "syntax") "."
    generateCpp     (syntaxName <.> "syntax") "."
    (exitCode, _, _) <- runShell ["g++", "-std=c++14", "-c", syntaxName <.> "cpp"]
    when (exitCode /= ExitSuccess) $ do
      error ("compile failure -- " ++ workspace </> syntaxName <.> "cpp")

-------------------------------------------------------------------------------

cleanupWorkspace :: FilePath -> FilePath -> IO ()
cleanupWorkspace workspace syntaxName = withCurrentDirectory workspace $ do
  return ()  -- should remove .o, .out?

-------------------------------------------------------------------------------

buildAndExecuteCpp :: FilePath -> FilePath -> IO (Maybe String)
buildAndExecuteCpp syntaxName target = (`runContT` return) $ callCC $ \exit -> do
  (exitCode1, _, _) <- runShell ["g++", "-std=c++14", "-c", target <.> "cpp"]
  when (exitCode1 /= ExitSuccess) (exit Nothing)

  (exitCode2, _, _) <- runShell ["g++", "-std=c++14", "-o", target <.> "out", syntaxName <.> "o", target <.> "o"]
  when (exitCode2 /= ExitSuccess) (exit Nothing)

  (exitCode3, output, _) <- runShell ["." </> target <.> "out"]
  when (exitCode3 /= ExitSuccess) $ do
    error ("unexpected runtime error in " ++ syntaxName </> target <.> "out")

  return (Just output)

buildAndExecuteHs :: FilePath -> FilePath -> IO (Maybe String)
buildAndExecuteHs syntaxName target = (`runContT` return) $ callCC $ \exit -> do
  tempdir <- lift getTemporaryDirectory
  (exitCode1, _, _) <- runShell ["ghc", "-fforce-recomp", "-outputdir", tempdir, "-o", target <.> "out", target <.> "hs"]
  when (exitCode1 /= ExitSuccess) (exit Nothing)

  (exitCode2, output, _) <- runShell ["." </> target <.> "out"]
  when (exitCode2 /= ExitSuccess) $ do
    error ("unexpected runtime error -- " ++ syntaxName </> target <.> "out")

  return (Just output)

-------------------------------------------------------------------------------

testWorkspace :: FilePath -> FilePath -> Spec
testWorkspace workspace syntaxName = do
  targets_ <- runIO $ listDirectory workspace
  let targets = filter (("test" `isPrefixOf`) . map toLower) targets_

  -- test .hs tests
  forM_ (filter (".hs" `isSuffixOf`) targets) $ \target -> do
    let targetName = dropEnd (length ".hs") target
    it ("correctly works for " ++ workspace </> target) $ do
      withCurrentDirectory workspace $ do
        result <- buildAndExecuteHs syntaxName targetName
        case getTestType target of
          "valid" -> do
            expected <- readFile (target <.> "expected")
            strip <$> result `shouldBe` Just (strip expected)
          "invalid" -> do
            result `shouldBe` Nothing
          testType -> error ("unknown test type -- " ++ testType)

  -- test .cpp tests
  forM_ (filter (".cpp" `isSuffixOf`) targets) $ \target -> do
    let targetName = dropEnd (length ".cpp") target
    it ("correctly works for " ++ workspace </> target) $ do
      withCurrentDirectory workspace $ do
        result <- buildAndExecuteCpp syntaxName targetName
        case getTestType target of
          "valid" -> do
            expected <- readFile (target <.> "expected")
            strip <$> result `shouldBe` Just (strip expected)
          "invalid" -> do
            result `shouldBe` Nothing

integratedSpec :: Spec
integratedSpec = describe "test examples in exmples/" $ do
    workspaces <- runIO getWorkspaces
    forM_ workspaces $ \(workspace, syntaxName) -> do
      beforeAll_ (setupWorkspace workspace syntaxName) $ do
        afterAll_ (cleanupWorkspace workspace syntaxName) $ do
          testWorkspace workspace syntaxName

-------------------------------------------------------------------------------
