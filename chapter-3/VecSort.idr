import Data.Vect

insert : Ord elem =>
         (x : elem) -> (xsSorted : Vect k elem) -> Vect (S k) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs

total insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted



{-

insSort [1, 3, 2]
{
  x = 1
  xsSorted = insSort [3, 2] in insert x [2, 3]
  insert = [1, 2, 3]
}

insSort [3, 2]
{
  x = 3
  xsSorted = insSort [2] in insert x [2]
  insert = [2, 3]
}

insSort [2]
{
  x = 2
  xsSorted = insSort [] in  insert x []
  insert = [x]
}

insSort []
{
  insSort = []
}

-}
