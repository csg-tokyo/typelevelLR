
import HelloDSL
import HelloDSLSemantics

main :: IO ()
main = runHelloDSL $ begin |> hello |> name "ymzk" |> end
