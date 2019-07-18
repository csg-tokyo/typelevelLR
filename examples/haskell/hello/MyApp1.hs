
import HelloDSL

main :: IO ()
main = print $ begin |> hello |> name "ymzk" |> end
