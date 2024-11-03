-- @file main.hs
-- @author Henry Wandover
-- @date 9/13/2024

import HUnit -- My growing testing framework, was just used for testing purposes in `main`.

-- Utility functions

-- @brief Return the proudct of all elements of a list.
-- @pre must of type Integer.
hwProduct :: [Int] -> Int
hwProduct [] = 1
hwProduct (it:list) = it * hwProduct list

-- @brief Return the length of a given list a.
hwLLen :: [a] -> Int
hwLLen [] = 0
hwLLen (_:list) = 1 + hwLLen list

-- @brief Compare two lists to see if they are identical.
hwLCmp :: [Int] -> [Int] -> Bool
hwLCmp [] [] = True
hwLCmp _ [] = False
hwLCmp [] _ = False
hwLCmp (it1:list1) (it2:list2) =
 if it1 == it2 then hwLCmp list1 list2
 else False


-- The actual assignment    |
--                          V


-- @brief Returns the list consisting of the prefix `list` of length n.
-- @param n : the length from first index to cut.
-- @param list : a list of Integers.
first_n :: Int -> [Int] -> [Int]
first_n _ [] = []
first_n idx (it:list) | idx == 0 = 
 first_n idx list
first_n idx (it:list) = 
 it : (first_n (idx - 1) list)

-- @brief Compute the product of the first n integers of l.
-- @param n : the length from first index to cut.
-- @param l : a list of Integers.
multiply_first_n :: Int -> [Int] -> Int
multiply_first_n _ [] = 1
multiply_first_n idx list =
 if idx > hwLLen list then hwProduct list
 else (hwProduct (first_n idx list))

-- @brief Returns a contiguous sublist of `list` that starts at the kth
-- and concludes at the nth element.
-- @param k : the first element of sublist.
-- @param n : the last element of sublist
-- @param list : the list being operated on.
between_k_and_n :: Int -> Int -> [Int] -> [Int]
between_k_and_n _ _ [] = []
between_k_and_n k n (it:list) | k > 1 = 
 between_k_and_n (k-1) (n-1) list -- skips elements until k
between_k_and_n k n (it:list) | n >= 1 =
 it : between_k_and_n 1 (n-1) list -- add elements until n
between_k_and_n k n (it:list) = 
 [] -- when n becomes 0, recursion stops

-- @brief Using between_k_and_n, takes a sub-list and finds the product of all the elements.

multiply_between :: Int -> Int -> [Int] -> Int
multiply_between _ _ [] = 1 -- base case
multiply_between k n list = 
 hwProduct (between_k_and_n k n list)

-- @brief For n > 2, the nth Twaalfskill number is the sum of the (n-1)st 
-- Twaalfskill number plus twice the (n-2)nd Twaalfskill number.
twaalfskill :: Int -> Int
twaalfskill 1 = 2
twaalfskill 2 = 5
twaalfskill n = (twaalfskill (n-1)) + (2*(twaalfskill (n-2)))

-- @brief Given a list, subsitutes each respsective number with its
-- twaalfskill's counterpart.
spreadTwaalfskill :: [Int] -> [Int]
spreadTwaalfskill [] = []
spreadTwaalfskill (it:list) =
 twaalfskill it : spreadTwaalfskill list

main :: IO()
main = do
 
 let firstN_tests = [
       ("3 [32,14,25,56,27,89]", [32,14,25], first_n 3 [32,14,25,56,27,89])]

 hwTestFunction "first_n" firstN_tests

 let multiplyFirstN_tests = [
       ("4 []", 1, multiply_first_n 4 []),
       ("3 [2,6,3,4,5,2]", 36, multiply_first_n 3 [2,6,3,4,5,2]),
       ("10 [2,6,3,4,5,2]", 1440, multiply_first_n 10 [2,6,3,4,5,2])]

 hwTestFunction "multiply_first_n" multiplyFirstN_tests

 let betweenKandN_tests = [
       ("2 4 [13,24,52,21]", [2,4,52], between_k_and_n 2 4 [13,2,4,52,21])]

 hwTestFunction "between_k_and_n" betweenKandN_tests

 let multiplyBetween_tests = [
       ("2 4 [13,24,52,21]", 14, multiply_between 2 4 [3,1,2,7,23,14])]

 hwTestFunction "multiply_between" multiplyBetween_tests

 let spreadTwaalfskill_tests = [
       ("1,3,5", [2,9,37], spreadTwaalfskill [1,3,5]),
       ("1,2,3,4", [2,5,9,19], spreadTwaalfskill [1,2,3,4])]

 hwTestFunction "spreadTwaalfskill" spreadTwaalfskill_tests
