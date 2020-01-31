
## TypeLevelLR

![test](https://github.com/csg-tokyo/typelevelLR/workflows/test/badge.svg)

- Fluent DSL Generator

- Supporting lauguages are:
    - Haskell
    - C++
    - Scala

### How to Install

Clone this repository and type stack install.

```sh
> git clone https://github.com/csg-tokyo/typelevelLR
> cd typelevelLR
> stack install
```

You can also use Docker:

```sh
> git clone https://github.com/csg-tokyo/typelevelLR
> cd typelevelLR
> docker build -t csg-tokyo/typelevellr:latest .
> docker run -v $(pwd):/workdir -it csg-tokyo/typelevellr:latest
```

### Usage:

1. Define your syntax

```sh
> nano hello.syntax
```
```hs
syntax helloDSL (Start) {
    simpleHello   : Start -> "hello"
    helloWithName : Start -> "hello" Name
    nameString    : Name  -> "name(String)"
}
```

2. Generate Fluent DSL Library

```sh
> typelevelLR --haskell
```

3. Import Fluent DSL Library

```sh
> nano MyApp.hs
```
```hs
import HelloDSL

main :: IO ()
main = print $ begin |> hello |> name "ymzk" |> end
```
```sh
> runghc MyApp.hs
HelloWithName (NameString "ymzk")
```

4. Define semantics

```sh
> nano HelloDSLSemantics.hs
```
```hs
module HelloDSLSemantics where

import HelloDSL

runHelloDSL :: Start -> IO ()
runHelloDSL (SimpleHello name) = putStrLn "Hello."
runHelloDSL (HelloWithName (NameString name)) = putStrLn ("Hello, " ++ name ++ "!!")
```

5. Run it

```sh
> nano MyApp.hs
```
```hs
module MyApp where

import HelloDSL
import HelloDSLSemantics

main :: IO ()
main = runHelloDSL $ begin |> hello |> name "ymzk" |> end
```
```sh
> runghc MyApp.hs
Hello, ymzk!!
```

### Details of Syntax Files

Will be written.
