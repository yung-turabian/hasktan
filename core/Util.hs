module Util (
    E(..)
) where


data E a = Ok a | Failed String
    deriving(Eq)
