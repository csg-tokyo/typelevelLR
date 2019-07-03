
module HelloDSLSemantics where

import HelloDSL

runHelloDSL :: Start -> IO ()
runHelloDSL SimpleHello = putStrLn "Hello!!"
runHelloDSL (HelloWithName (NameString name)) = putStrLn ("Hello, " ++ name ++ ".")
