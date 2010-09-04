import Data.Tree
import qualified List

{-
  Searching tree with alpha-beta algorithm.
  This program ends.
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
negamax tree = negamax' tree (-255) 255
negamax' tree alpha beta = 
  if rootLabel tree /= noleaf then rootLabel tree
  else negamaxLoop (subForest tree) alpha beta
negamaxLoop trees alpha beta =
  case trees of
    [] -> alpha
    (x:xs) -> if s >= beta then s else negamaxLoop xs (max alpha s) beta
       where s = negate $ negamax' x (-beta) (-alpha)

main = print $ negamax mytree
