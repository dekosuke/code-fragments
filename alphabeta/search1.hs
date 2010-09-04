import Data.Tree
import qualified List

{-
  Searching tree with shortest-beta cut.
  This program never terminates.
-}

noleaf = 255
treeMaker 0 = (noleaf, [1,2])
treeMaker 1 = (-5, [])
treeMaker 2 = (noleaf, [3,4])
treeMaker 3 = (8, [])
treeMaker 4 = (noleaf, [5,6])
treeMaker 5 = (-3, [])
treeMaker 6 = (noleaf, [7..])
treeMaker 7 = (4, [])
treeMaker _ = (6, [])
mytree = unfoldTree treeMaker 0

--mytree = [5,[8,[3,[4..]]]]

negamax :: Tree Integer -> Integer
negamax tree = maximum $ negamax' tree
maxleq pot [] = False
maxleq pot (x:xs) = (x>=pot) ||  maxleq pot xs
omit pot [] = []
omit pot (x:xs) =
  if maxleq pot x then omit pot xs
  else maximum x : omit (maximum x) xs
mapnegatemax [] = []
mapnegatemax (x:xs) = map negate (maximum x : omit (maximum x) xs)
negamax' tree =
  if rootLabel tree /= noleaf then [(rootLabel tree)]
  else mapnegatemax $ map negamax' (subForest tree) 

main = print $ negamax mytree
