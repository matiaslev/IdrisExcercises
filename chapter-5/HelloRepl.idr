module Main

getWelcome : String -> String
getWelcome x = "Hello " ++ x ++ "!\n"

main : IO ()
main = repl "Enter your name: " getWelcome
