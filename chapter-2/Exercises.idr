module Excercises

export
palindrome : Nat -> String -> Bool
palindrome m x = let y = reverse x
                     result = toLower x == toLower y in
                     if length x > m then result else False

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)


top_ten : Ord a => List a -> List a
top_ten list = take 10 (reverse (sort list))

over_length : Nat -> List String -> Nat
over_length x str = length (filter (> x) (map length str))
