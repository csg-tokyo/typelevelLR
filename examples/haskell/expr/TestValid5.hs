
import Expr

main :: IO ()
main = print $ begin |> lp |> lp |> lp |> num 1 |> rp |> rp |> rp |> end
