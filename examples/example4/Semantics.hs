
module Semantics where

import Example4

runExample4 :: Start -> IO ()
runExample4 SimpleHello = putStrLn "Hello!!"
runExample4 (HelloWithName (NameString name)) = putStrLn ("Hello, " ++ name ++ ".")
