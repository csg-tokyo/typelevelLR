
import OopsDSL

printOops :: Oops -> IO ()
printOops oops = goOops oops >> putStrLn ""
  where goOops (AddPs os) = goOs os >> putStr "ps"
        goOs   (AddO  os) = putStr "o" >> goOs os
        goOs   EndOs      = return ()

main :: IO ()
main = printOops $ begin o o ps end
