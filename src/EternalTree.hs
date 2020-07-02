-- "Eternal tree" (c) by Ignacio Slater M.

-- "Eternal tree" is licensed under a
-- Creative Commons Attribution 4.0 International License.

-- You should have received a copy of the license along with this
-- work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

-- v1.0.0-b.3
module EternalTree where

import           Data.Tree

type Depth = Int
type Children = Int
data Tree' a = Node' a [Tree' a] deriving Show

{-|
Generates a tree with root r and where every child is f(r)
-}
itTree :: (a -> [a]) -> a -> Tree' a
itTree f r = Node' r (map (itTree f) (f r))

{-| Takes the first elements of a tree given a depth and a number of children per node. -}
treeTake :: Depth -> Children -> Tree' a -> Tree' a
treeTake 0 _ (Node' x _) = Node' x []
treeTake depth children (Node' x ys) =
  Node' x $ take children $ map (treeTake (depth - 1) children) ys

toDataTree :: Tree' Int -> Tree String
toDataTree (Node' v ts) = Node (show v) $ map toDataTree ts

successors :: Int -> [Int]
successors n = next : successors next where next = n + 1

infTree :: Tree' Int
infTree = itTree successors 0
