-- Henry Wandover
-- CMSC 305, Lab 3
-- Due: Friday, September 27th, 2024

import GHUT

data BST = Empty | Node Int BST BST
 deriving Show


-- @brief Which takes an Int and a BST and returns a new BST that looks like the original except that a copy of the Int has been inserted into the correct location according to the rules of a binary search tree.
bstInsert :: Int -> BST -> BST
bstInsert toAdd Empty = Node toAdd Empty Empty -- No root
bstInsert toAdd (Node val left right) | toAdd < val = Node val (bstInsert toAdd left) right -- left subtree
bstInsert toAdd (Node val left right) | toAdd >= val = Node val left (bstInsert toAdd right) -- right subtree


-- @brief When given an Int and a BST determines whether that Int is present in the BST.
bstFind :: Int -> BST -> Bool
bstFind _ Empty = False
bstFind toFind (Node val _ _) | toFind == val = True
bstFind toFind (Node val _ right) | toFind > val = bstFind toFind right
bstFind toFind (Node val left _) | toFind < val = bstFind toFind left


-- @brief returns the max Int in the BST.
bstFindMax :: BST -> Int
bstFindMax (Node val Empty Empty) = val
bstFindMax (Node val _ Empty) = val
bstFindMax (Node val _ right) = bstFindMax right


-- @brief returns the min Int in the BST.
bstFindMin :: BST -> Int
bstFindMin (Node val Empty Empty) = val
bstFindMin (Node val Empty _) = val
bstFindMin (Node _ left _) = bstFindMin left


-- @brief creates a new BST that is similar to the original only with the supplied Int removed, if it is present.
bstDelete :: Int -> BST -> BST
bstDelete _ Empty = Empty
bstDelete toDelete (Node val left right) | toDelete < val = Node val (bstDelete toDelete left) right
bstDelete toDelete (Node val left right) | toDelete > val = Node val left (bstDelete toDelete right)
bstDelete _ (Node _ Empty Empty) = Empty
bstDelete _ (Node _ left Empty) = left
bstDelete _ (Node _ Empty right) = right


-- @brief Creates a new BST that is similar to the original only with the max value removed.
bstDeleteMax :: BST -> BST
bstDeleteMax tree = bstDelete (bstFindMax tree) tree


-- @brief Creates a new BST that is similar to the original only with the min value removed.
bstDeleteMin :: BST -> BST
bstDeleteMin tree = bstDelete (bstFindMin tree) tree


-- @brief Reverses a list.
hwListReverse :: [a] -> [a]
hwListReverse [] = []
hwListReverse (it:list) = hwListReverse list ++ [it]


-- @brief Util function for `bstFromInts` to still be recursive but abstracts the need to reverse.
_bstTraverseInts :: [Int] -> BST
_bstTraverseInts [] = Empty
_bstTraverseInts (it:list) = bstInsert it (_bstTraverseInts list)


-- @brief Creates a BST from a list of Ints where the Ints are added in the order in which they appear in the list.
-- @note Need to reverse so it can be entered in order.
bstFromInts :: [Int] -> BST
bstFromInts list = _bstTraverseInts (hwListReverse list)


-- @brief Returns a list of the elements of a BST in ascending order.
bstToInts :: BST -> [Int]
bstToInts Empty = []
bstToInts tree = [(bstFindMin tree)] ++ bstToInts (bstDeleteMin tree) 


-- @brief Util function for list comparison.
hwListCmp :: (Num a, Eq a) => [a] -> [a] -> Bool
hwListCmp [] [] = True
hwListCmp _ [] = False
hwListCmp [] _ = False
hwListCmp (it1:list1) (it2:list2) =
 if it1 == it2 then hwListCmp list1 list2
 else False


-- @brief Compares whether two BSTs are the same.
bstCmp :: BST -> BST -> Bool
bstCmp tree1 tree2 = hwListCmp (bstToInts tree1) (bstToInts tree2)

-- Iterative loop passing an array? then do if checks?
test_bstInsert :: TestCase
test_bstInsert = TestCase "bstFind" $ do
  let tree = bstInsert 10 Empty
  hwAssertTrue (bstFind 10 tree)
  {-let tree2 = bstInsert 5 tree
  hwAssertTrue (bstFind 5 tree2)
  let tree3 = bstInsert 15 tree3
  hwAssertTrue (bstFind 15 tree3)
-}

main :: IO ()
main = do
  hwTestRunner [test_bstInsert]
