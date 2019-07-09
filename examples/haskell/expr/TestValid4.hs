
import Expr

main :: IO ()
main = print $ begin |> lp |> num 1 |> add |> num 2 |> rp |> mul |> num 3 |> end
