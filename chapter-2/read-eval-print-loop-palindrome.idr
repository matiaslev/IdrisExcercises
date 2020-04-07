module Main

palindrome : Nat -> String -> Bool
palindrome m x = let y = reverse x
                     result = toLower x == toLower y in
                     if length x > m then result else False

main : IO ()
main = repl "enter a word: " show_palindrome
  where
    show_palindrome : String -> String
    show_palindrome x = show (palindrome x) ++ "\n"
