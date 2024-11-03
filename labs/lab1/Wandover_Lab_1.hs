-- @file test.hs
-- @author Henry Wandover
-- @date September 6th, 2024

import HsUnit

-- @brief Computes the sum of integers from k up to n.
addBetween :: Int -> Int -> Int
addBetween 0 0 = 0
addBetween k n = 
 if n == k then k
 else if k < n then k + addBetween (k + 1) n -- if first arg is smaller
 else k + addBetween n (k - 1)

-- @brief Computes the sum of integers from 0 to n.
addAll :: Int -> Int
addAll 0 = 0
addAll n = addBetween 0 n


-- @brief Computes the sum of all divisors of n, between k and n - 1.
addDivisors :: Int -> Int -> Int
addDivisors _ 0 = -1 -- Can't divide by 0
addDivisors k n =
 if k == (n - 1) then 0 -- base case

 -- The anonymous function below, checks whether the divisor does so without a remainder; if no remainder return the divisor to add to sum, else return 0
 else 
 ((\divisor dividend -> if (mod dividend divisor) == 0 then divisor else 0) k n) + addDivisors (k + 1) n

-- @brief Returns true for a positive integer n, precisely when the sum of the
-- positive factors of n less than n is also n.
perfectNumber :: Int -> Bool
perfectNumber n = (addDivisors 1 n) == n

-- @brief For n > 2, the nth Twaalfskill number is the sum of the (n-1)st 
-- Twaalfskill number plus twice the (n-2)nd Twaalfskill number.
twaalfskill :: Int -> Int
twaalfskill 1 = 2
twaalfskill 2 = 5
twaalfskill n = (twaalfskill (n-1)) + (2*(twaalfskill (n-2)))

main :: IO()
main = do
 -- Test cases
 hwAssertEqual "addBetween(5,8)" 26 (addBetween 5 8)
 hwAssertEqual "addBetween(3,5)" 12 (addBetween 3 5) 
 hwAssertEqual "addBetween(0,0)" 0 (addBetween 0 0) 
 hwAssertEqual "addBetween(5,0)" 15 (addBetween 5 0)
 hwAssertEqual "addBetween(5,3)" 12 (addBetween 5 3)

 hwAssertEqual "addAll(5)" 15 (addAll 5)
 hwAssertEqual "addAll(0)" 0 (addAll 0)
 hwAssertEqual "addAll(2)" 3 (addAll 2)
 hwAssertEqual "addAll(8)" 36 (addAll 8)

 hwAssertEqual "addDivisors(2,6)" 5 (addDivisors 2 6)
 hwAssertEqual "addDivisors(2,8)" 6 (addDivisors 2 8)
 hwAssertEqual "addDivisors(1,12)" 16 (addDivisors 1 12)
 hwAssertEqual "addDivisors(5,28)" 21 (addDivisors 5 28)
 hwAssertEqual "addDivisors(3,17)" 0 (addDivisors 3 17) -- prime

 hwAssertTrue "perfectNumber(6)" (perfectNumber 6)
 hwAssertFalse "perfectNumber(5)" (perfectNumber 5)

 hwAssertEqual "twaalfskill(1)" 2 (twaalfskill 1)
 hwAssertEqual "twaalfskill(2)" 5 (twaalfskill 2)
 hwAssertEqual "twaalfskill(3)" 9 (twaalfskill 3)
 hwAssertEqual "twaalfskill(4)" 19 (twaalfskill 4)
 hwAssertEqual "twaalfskill(5)" 37 (twaalfskill 5)
