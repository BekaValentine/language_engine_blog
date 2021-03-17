import ABT
import Graph





main :: IO ()
main = do
  print t
  print (abtToGraph t)
  where
    t :: ABT
    t = Con "Foo" [scope ["x","y"] (Con "Bar" [scope [] (Var (Free "y"))])]