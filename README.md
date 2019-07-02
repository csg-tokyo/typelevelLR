
## TypeLevelLR

- Fluent DSL Generator

- Supporting lauguages are:
    - Haskell
    - C++
    - Scala

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
            nameString    : Name  -> "name(String)"
        }

2. Generate Fluent DSL Library

        > typelevelLR --hs hello.syntax

3. Import Fluent DSL Library

        > nano MyApp.hs
        import HelloDSL

        main :: IO ()
        main = print $ begin |> hello |> name "ymzk" |> end

        > runghc MyApp.hs
        HelloWithName (NameString "ymzk")

4. Define semantics

        > nano HelloDSLSemantics.hs
        module HelloDSLSemantics where

        import HelloDSL

        runHelloDSL :: Start -> IO ()
        runHelloDSL (SimpleHello name) = putStrLn "Hello."
        runHelloDSL (HelloWithName (NameString name)) = putStrLn ("Hello, " ++ name ++ "!!")

5. Run it

        > nano MyApp.hs
        module MyApp where

        import HelloDSL
        import HelloDSLSemantics

        main :: IO ()
        main = runHelloDSL $ begin hello str "ymzk" end

        > runghc MyApp.hs
        Hello, ymzk!!

### Details of Syntax Files

Will be written.
