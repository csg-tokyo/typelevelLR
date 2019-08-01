
{-# LANGUAGE FlexibleContexts #-}

module TellChain where

import qualified Syntax as S
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Writer
import Utility

-------------------------------------------------------------------------------

newtype TellChainEnv = TellChainEnv (Map.Map String [String])

tellChainEnvLookup :: (MonadState TellChainEnv m) => String -> m String
tellChainEnvLookup key = state $ \(TellChainEnv bindings) ->
  case Map.lookup key bindings of
    Nothing -> error ("unknown type found -- " ++ key)
    Just [] -> error ("no remaining value for " ++ key)
    Just (v : vs) -> (v, TellChainEnv (Map.insert key vs bindings))

-------------------------------------------------------------------------------

tellChainWith :: (MonadWriter (Endo String) m) =>
               (S.Terminal -> m any) -> String -> [S.Terminal] -> m ()
tellChainWith tellTerminal sep chain = do
  tellTerminal (S.UserTerminal "begin" [])
  tells sep
  mapMWithSep_ (tells sep) tellTerminal chain
  tells sep
  tellTerminal S.EndOfInput
  return ()

tellHaskellChain :: (MonadWriter (Endo String) m, MonadState TellChainEnv m) =>
                    [S.Terminal] -> m ()
tellHaskellChain = tellChainWith tellHaskellTerminal " |> "

tellCppChain :: (MonadWriter (Endo String) m, MonadState TellChainEnv m) =>
                [S.Terminal] -> m ()
tellCppChain = tellChainWith tellCppTerminal "->"

tellScalaChain :: (MonadWriter (Endo String) m, MonadState TellChainEnv m) =>
                  [S.Terminal] -> m ()
tellScalaChain = tellChainWith tellScalaTerminal "."

-------------------------------------------------------------------------------

tellTerminalWith :: (MonadWriter (Endo String) m, MonadState TellChainEnv m) =>
                    (m () -> m ()) -> m () -> S.Terminal -> m ()
tellTerminalWith parenthesize sep (S.UserTerminal name params) = do
  tells name
  parenthesize $ forMWithSep_ sep params $ \param -> do
    tellChainEnvLookup param >>= tells
tellTerminalWith parenthesize sep S.EndOfInput = do
  tells "end" >> parenthesize (return ())


tellHaskellTerminal :: (MonadWriter (Endo String) m, MonadState TellChainEnv m) =>
                       S.Terminal -> m ()
tellHaskellTerminal = tellTerminalWith (\m -> tells " " *> m) (tells " ")

tellCppTerminal :: (MonadWriter (Endo String) m, MonadState TellChainEnv m) =>
                   S.Terminal -> m ()
tellCppTerminal = tellTerminalWith (\m -> tells "(" *> m *> tells ")") (tells ", ")

tellScalaTerminal :: (MonadWriter (Endo String) m, MonadState TellChainEnv m) =>
                     S.Terminal -> m ()
tellScalaTerminal = tellTerminalWith (\m -> tells "(" *> m *> tells ")") (tells ", ")

-------------------------------------------------------------------------------

strings :: [String]
strings = concat (zipWith (++) [replicateM i lowers | i <- [1 ..]]
                               [replicateM i uppers | i <- [1 ..]])
  where lowers = "abcdefghijklmnopqrstuvwxyz"
        uppers = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

haskellTellChainEnv :: TellChainEnv
haskellTellChainEnv = TellChainEnv $ Map.fromList $
  [("Int"    , map show [1 ..]),
   ("Integer", map show [1 ..]),
   ("String" , map show strings),
   ("Bool"   , cycle ["True", "False"])]

cppTellChainEnv :: TellChainEnv
cppTellChainEnv = TellChainEnv $ Map.fromList $
  [("int"        , map show [1 ..]),
   ("std::string", map show strings),
   ("bool"       , cycle ["true", "false"])]

scalaTellChainEnv :: TellChainEnv
scalaTellChainEnv = TellChainEnv $ Map.fromList $
  [("Int"    , map show [1 ..]),
   ("String" , map show strings),
   ("Boolean", cycle ["true", "false"])]

-------------------------------------------------------------------------------

tellHaskellSource :: (MonadWriter (Endo String) m) => String -> String -> [S.Terminal] -> m ()
tellHaskellSource _modName libName chain = (`evalStateT` haskellTellChainEnv) $ do
  tellsLn  ""
  tellsLn ("import " ++ pascalCase libName)
  tellsLn  ""
  tellsLn  "main :: IO ()"
  tells   ("main = print $ ")
  tellHaskellChain chain
  tellsLn ""

tellCppSource :: (MonadWriter (Endo String) m) => String -> String -> [S.Terminal] -> m ()
tellCppSource _modName libName chain = (`evalStateT` cppTellChainEnv) $ do
  tellsLn  ""
  tellsLn  "#include <iostream>"
  tellsLn ("#include \"" ++ libName ++ ".hpp\"")
  tellsLn ("using namespace " ++ libName ++ ";")
  tellsLn  ""
  tellsLn  "int main() {"
  tells    "    auto parseTree = "
  tellCppChain chain
  tellsLn  ";"
  tellsLn  "    std::cout << parseTree << std::endl;"
  tellsLn  "}"

tellScalaSource :: (MonadWriter (Endo String) m) => String -> String -> [S.Terminal] -> m ()
tellScalaSource modName libName chain = (`evalStateT` scalaTellChainEnv) $ do
  tellsLn  ""
  tellsLn ("object " ++ modName ++ " {")
  tellsLn ("import " ++ libName ++ "._")
  tellsLn  "  def main(args: Array[String]) = {"
  tells   ("    val parseTree = ")
  tellScalaChain chain
  tellsLn  ""
  tellsLn  "    println(parseTree)"
  tellsLn  "  }"
  tellsLn  "}"

-------------------------------------------------------------------------------
