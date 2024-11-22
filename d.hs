((\y -> y + 3) :: Int -> Int) (((\y -> if y == True then 4 else 2) :: Bool -> Int) (((\y -> y /= 7) :: Int → Bool) 3))
