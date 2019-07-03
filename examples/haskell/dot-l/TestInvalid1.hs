
import DotLanguage

main :: IO ()
main = print $ begin
    |> digraph "small_graph"
        |> node "A" |> shape "rectangle"
        |> node "B" |> node "C" |> shape "doublecircle"
        |> edge "A" |> to "B" |> shape "dotted"
        |> edge "A" |> and_ "B" |> to "C"
    |> end
