
module HelloDSLSemantics where

import HelloDSL

runHelloDSL :: Start -> IO ()
runHelloDSL SimpleHello = putStrLn "Hello!!"
runHelloDSL (HelloWithName name) = putStrLn ("Hello, " ++ name ++ ".")
