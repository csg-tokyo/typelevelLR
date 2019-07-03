
import HelloDSL
import HelloDSLSemantics

main :: IO ()
main = runHelloDSL $ begin |> hello |> end
