
## TypeLevelLR

- Fluent DSL Generator

- Supporting lauguages are:
    - Haskell
    - C++

### How to Install

    	> git clone https://github.com/ymzk/typelevelLR
	> cd typelevelLR
        > stack install

### Usage:

1. Define your syntax

        > nano hello.syntax
        syntax helloDSL (Start) {
            simpleHello   : Start -> "hello"
            helloWithName : Start -> "hello" Name
            nameString    : Name  -> str
        }

1. Generate Fluent DSL Library

        > typelevelLR -hs hello.syntax

1. Import Fluent DSL Library

        > nano MyApp.hs
        import HelloDSL

        main :: IO ()
        main = print $ begin hello str "ymzk" end

        > runghc MyApp.hs
        HelloWithName (NameString "ymzk")

1. Define semantics

        > nano HelloDSLSemantics.hs
        module HelloDSLSemantics where

        import HelloDSL

        runHelloDSL :: Start -> IO ()
        runHelloDSL SimpleHello name) = putStrLn "Hello!!"
        runHelloDSL (HelloWithName (NameString name)) = putStrLn ("Hello, " ++ name ++ ".")

1. Run it

        > nano MyApp.hs
        module MyApp where

        import HelloDSL
        import HelloDSLSemantics

        main :: IO ()
        main = runHelloDSL $ begin hello str "ymzk" end

        > runghc MyApp.hs
        Hello, ymzk.

### Detail of Syntax File

Under constructing
