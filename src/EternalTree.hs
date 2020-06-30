-- "Eternal tree" (c) by Ignacio Slater M.

-- "Eternal tree" is licensed under a
-- Creative Commons Attribution 4.0 International License.

-- You should have received a copy of the license along with this
-- work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

-- v1.0.0-rc.1
module EternalTree where

data Tree a = Node a [Tree a] deriving Show

{-|
Generates a tree with root r and where every child is f(r)
-}
itTree :: (a -> [a]) -> a -> Tree a
itTree f r = Node r (map (itTree f) (f r))

treeTake :: Int -> Int -> Tree a -> Tree a
treeTake 0 _ (Node x _) = Node x []
treeTake maxDepth maxLength (Node x ys) =
  Node x $ take maxLength $ map (treeTake (maxDepth - 1) maxLength) ys

successors :: Int -> [Int]
successors n = next : successors next where next = n + 1

infTree :: Tree Int
infTree = itTree successors 0