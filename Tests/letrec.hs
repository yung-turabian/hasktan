letrec f =
  (\x -> if x == 0 then 1 else f (x - 1) * x) :: Int -> Int
in f 5
