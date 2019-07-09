
import Expr

main :: IO ()
main = print $ begin |> num 1 |> add |> num 2 |> rp |> end
