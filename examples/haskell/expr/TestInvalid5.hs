
import Expr

main :: IO ()
main = print $ begin |> rp |> num 1 |> add |> num 2 |> lp |> end
