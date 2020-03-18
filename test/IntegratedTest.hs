
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
import Generate                (generateHaskell, generateCpp, generateScala)

-------------------------------------------------------------------------------

cCompiler :: String
cCompiler = "g++"

ccOptions :: [String]
ccOptions = ["-std=c++17"]

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

setupHaskell :: FilePath -> FilePath -> IO ()
setupHaskell workspace syntaxName = do
  withCurrentDirectory workspace $ do
    unlessM (doesFileExist (syntaxName <.> "syntax")) $ do
      error "no syntax file found"
    generateHaskell (syntaxName <.> "syntax") "."

setupCpp :: FilePath -> FilePath -> IO ()
setupCpp workspace syntaxName = do
  withCurrentDirectory workspace $ do
    unlessM (doesFileExist (syntaxName <.> "syntax")) $ do
      error "no syntax file found"
    generateCpp (syntaxName <.> "syntax") "."

setupScala :: FilePath -> FilePath -> IO ()
setupScala workspace syntaxName = do
  withCurrentDirectory workspace $ do
    unlessM (doesFileExist (syntaxName <.> "syntax")) $ do
      error "no syntax file found"
    generateScala (syntaxName <.> "syntax") "."
    writeFile "build.sbt" "scalaVersion := 2.12.4"

-------------------------------------------------------------------------------

cleanupCpp :: FilePath -> FilePath -> IO ()
cleanupCpp workspace syntaxName = withCurrentDirectory workspace $ do
  runShell ["rm", "*.o", "*.out"]
  return ()

cleanupHaskell :: FilePath -> FilePath -> IO ()
cleanupHaskell workspace syntaxName = withCurrentDirectory workspace $ do
  runShell ["rm", "*.out"]
  return ()

cleanupScala :: FilePath -> FilePath -> IO ()
cleanupScala workspace syntaxName = withCurrentDirectory workspace $ do
  runShell ["rm", "*.out", "*.jar", "*.class"]
  return ()

-------------------------------------------------------------------------------

buildAndExecuteCpp :: FilePath -> FilePath -> IO (Maybe String)
buildAndExecuteCpp syntaxName target = (`runContT` return) $ callCC $ \exit -> do
  (exitCode1, _, _) <- runShell ([cCompiler] ++ ccOptions ++ ["-c", target <.> "cpp"])
  when (exitCode1 /= ExitSuccess) (exit Nothing)

  (exitCode2, _, _) <- runShell ([cCompiler] ++ ccOptions ++ ["-o", target <.> "out", syntaxName <.> "o", target <.> "o"])
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

buildAndExecuteScala :: FilePath -> FilePath -> IO (Maybe String)
buildAndExecuteScala syntaxName target = (`runContT` return) $ callCC $ \exit -> do
  _ <- runShell ["scalac", syntaxName <.> "scala", target <.> "scala"]
  (exitCode1, output, _) <- runShell ["scala", target]
  when (exitCode1 /= ExitSuccess) (exit Nothing)
  return (Just output)

-------------------------------------------------------------------------------

testWorkspace :: FilePath -> (FilePath -> FilePath -> IO (Maybe String)) -> FilePath -> FilePath -> Spec
testWorkspace extension execute workspace syntaxName = do
  targets_ <- runIO $ listDirectory workspace
  let targets = filter (("test" `isPrefixOf`) . map toLower) targets_
  forM_ (filter (extension `isSuffixOf`) targets) $ \target -> do
    let targetName = dropEnd (length extension) target
    it ("correctly works for " ++ workspace </> target) $ do
      withCurrentDirectory workspace $ do
        result <- execute syntaxName targetName
        case getTestType target of
          "valid" -> do
            expected <- readFile (target <.> "expected")
            fmap strip result `shouldBe` Just (strip expected)
          "invalid" -> do
            result `shouldBe` Nothing
          testType -> error ("unknown test type -- " ++ testType)

testHaskell :: FilePath -> FilePath -> Spec
testHaskell = testWorkspace ".hs" buildAndExecuteHs

testCpp :: FilePath -> FilePath -> Spec
testCpp = testWorkspace ".cpp" buildAndExecuteCpp

testScala :: FilePath -> FilePath -> Spec
testScala = testWorkspace ".scala" buildAndExecuteScala

-------------------------------------------------------------------------------

isValidWorkspace :: FilePath -> FilePath -> IO Bool
isValidWorkspace base path = (`runContT` return) $ callCC $ \exit -> do
  unlessM (lift (doesDirectoryExist (base </> path))) $ do
    exit False
  unlessM (lift (doesFileExist (base </> path </> path <.> "syntax"))) $ do
    exit False
  return True

-------------------------------------------------------------------------------

integratedSpec :: Spec
integratedSpec = describe "test examples in exmples/" $ do
  languages <- runIO (listDirectory "examples")
  forM_ languages $ \lang -> do
    let basePath = "examples" </> lang
    workspaces <- runIO (listDirectory basePath >>= filterM (isValidWorkspace basePath))
    forM_ workspaces $ \workspace_ -> do
      let syntaxName = workspace_
      let workspace = basePath </> workspace_
      case lang of
        "haskell" -> do
          runIO (putStrLn ("workspace: " ++ workspace ++ ", syntaxName: " ++ syntaxName))
          beforeAll_ (setupHaskell workspace syntaxName) $ do
            afterAll_ (cleanupHaskell workspace syntaxName) $ do
              testHaskell workspace syntaxName
        "cpp" -> do
          beforeAll_ (setupCpp workspace syntaxName) $ do
            afterAll_ (cleanupCpp workspace syntaxName) $ do
              testCpp workspace syntaxName
        "scala" -> do
          runIO (putStrLn ("workspace: " ++ workspace ++ ", syntaxName: " ++ syntaxName))
          beforeAll_ (setupScala workspace syntaxName) $ do
            afterAll_ (cleanupScala workspace syntaxName) $ do
              testScala workspace syntaxName
        _ -> do
          runIO (putStrLn ("warning: unknown lang -- " ++ lang))

-------------------------------------------------------------------------------
