
import HelloDSL

main :: IO ()
main = print $ begin |> hello |> name "name" |> end
