
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Writer
import Data.Monoid
import System.Environment
import System.Console.GetOpt
import Control.Monad.Reader

-------------------------------------------------------------------------------

tells :: (MonadWriter (Endo String) m) => String -> m ()
tells str = tell (Endo (str ++))

tellsLn :: (MonadWriter (Endo String) m) => String -> m ()
tellsLn str = tells (str ++ "\n")

exec :: Writer (Endo String) any -> String
exec = (`appEndo` "") . execWriter

-------------------------------------------------------------------------------

nodeNames :: [String]
nodeNames = [1 ..] >>= \n -> replicateM n alphabets
  where alphabets = ['A'..'Z']

tellNodes :: (MonadWriter (Endo String) m) => [String] -> [String] -> m ()
tellNodes options names = do
  forM_ (zip (cycle options) names) $ \(option, name) -> do
    tellsLn (".node(" ++ show name ++ ")" ++ option)

edges :: [String] -> Int -> [String] -> [(String, String, String)]
edges options d nodes = go nodes
  where go [] = []
        go (src : rest) = [(src, dst, opt) | (opt, dst) <- zip (cycle options) (take d (rest ++ nodes))] ++ go rest

tellEdges :: (MonadWriter (Endo String) m) => [String] -> Int -> [String] -> m ()
tellEdges options d nodes = do
  forM_ (edges options d nodes) $ \(src, dst, opt) -> do
    tellsLn (".edge(" ++ show src ++ ").to(" ++ show dst ++ ")" ++ opt)

tellGraph :: (MonadWriter (Endo String) m, MonadReader Config m) => m ()
tellGraph = do
  modName <- asks configModName
  graphName <- asks configGraphName
  n <- asks configN
  d <- asks configD
  nodeOptions <- asks configNodeOpts
  edgeOptions <- asks configEdgeOpts
  let chainLength = 1 + n + 2 * d * n + 1
  let nodes = take n nodeNames
  tellsLn $ ""
  tellsLn $ "// " ++ show chainLength
  tellsLn $ ""
  tellsLn $ "object " ++ modName ++ " {"
  tellsLn $ "  import exam1._"
  tellsLn $ ""
  tellsLn $ "  def main(args : Array[String]) = {"
  tellsLn $ "    val graph = begin()"
  tellsLn $ "          .digraph(" ++ show graphName ++ ")"
  tellNodes nodeOptions nodes
  tellEdges edgeOptions d nodes
  tellsLn $ "          .end()"
  tellsLn $ "    println(graph)"
  tellsLn $ "  }"
  tellsLn $ "}"

-------------------------------------------------------------------------------

data Config = Config { configN          :: Int,
                       configD          :: Int,
                       configNodeOpts   :: [String],
                       configEdgeOpts   :: [String],
                       configModName    :: String,
                       configGraphName  :: String,
                       configOutputFile :: String }

defaultConfig :: Config
defaultConfig = Config 10 2 [""] [""] "test" "test" "test.scala"

data Flag = FlagN          Int
          | FlagD          Int
          | FlagNodeOpts   [String]
          | FlagEdgeOpts   [String]
          | FlagModName    String
          | FlagGraphName  String
          | FlagOutputFile FilePath

flagsToConfig :: [Flag] -> Config
flagsToConfig = foldl go defaultConfig
  where go config (FlagN n) = config { configN = n }
        go config (FlagD d) = config { configD = d }
        go config (FlagNodeOpts opts) = config { configNodeOpts = opts }
        go config (FlagEdgeOpts opts) = config { configEdgeOpts = opts }
        go config (FlagModName name) = config { configModName = name }
        go config (FlagGraphName name) = config { configGraphName = name }
        go config (FlagOutputFile file) = config { configOutputFile = file }

options :: [OptDescr Flag]
options = [Option ['n'] [] (ReqArg (FlagN . read) "N") "node count",
           Option ['d'] [] (ReqArg (FlagD . read) "D") "graph digree",
           Option ['m'] [] (ReqArg FlagModName "MOD") "module name",
           Option ['g'] [] (ReqArg FlagGraphName "NAME") "graph name",
           Option ['o'] [] (ReqArg FlagModName "OUTPUT") "output file"]

main :: IO()
main = do args <- getArgs
          case getOpt Permute options args of
            (flags, [], []) -> do
              let config = flagsToConfig flags
              let src = exec (runReaderT tellGraph config)
              writeFile (configOutputFile config) src
            (_, x : _, []) -> do
              putStrLn $ "unrecognized option -- " ++ x
            (_, _, errs) -> error (concat errs)
