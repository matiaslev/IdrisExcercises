import Data.Vect

total my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

{-
Split definitions ctrl+ cmnd + A
split cases ctrl+ cmnd + C
refine hole ctrl+ cmnd + S
-}

total my_reverse : List a -> List a
my_reverse [] = []
my_reverse (head :: tail) = my_reverse tail ++ [head]

total my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (head :: tail) = f head :: my_map f tail

total my_map_vect : (a -> b) -> Vect n a -> Vect n b
my_map_vect f [] = []
my_map_vect f (x :: xs) = f x :: my_map_vect f xs
