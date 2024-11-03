{-
 - @file HEW.hs
 - @author Henry Wandover / yung-turabian
 - @date September 16th, 2024
 - @brief An extension to the standard Haskell library.
-}

module HEW (
 hwListCmp,
) where


-- @brief Compare lists.
hwListCmp :: (Num a, Eq a) => [a] -> [a] -> Bool
hwListCmp [] [] = True
hwListCmp _ [] = False
hwListCmp [] _ = False
hwListCmp (it1:list1) (it2:list2) =
 if it1 == it2 then hwListCmp list1 list2
 else False

-- @brief Return the length of a given list a.
hwLLen :: [a] -> Int
hwLLen [] = 0
hwLLen (_:list) = 1 + hwLLen list

-- @brief Solves factorials
hwFact :: Int -> Int
hwFact 0 = 1
hwFact n = n * hwFact(n - 1)

-- @brief Checks if a is a member of list.
hwMember :: (Eq a) => a -> [a] -> Bool
hwMember _ [] = False
hwMember item (i:is) = 
 (item == i) || (hwMember item is)

-- @brief
hwPopItem :: Int -> [Int] -> [Int]
hwPopItem _ [] = []
hwPopItem n (it:list) | n == it = 
 hwPopItem n list
hwPopItem n (it:list) = 
 it : (hwPopItem n list)

-- @brief
hwPopFront :: [Int] -> [Int]
hwPopFront [] = []
hwPopFront list = hwPopItem 1 list

-- @brief Sum all Ints in a list of Ints
hwSum :: [Int] -> Int
hwSum [] = 0
hwSum (n:ns) = n + hwSum ns
