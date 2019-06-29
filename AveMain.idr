module Main
import Average

showAverage : String -> String
showAverage str = "Average : " ++ (show (average str)) ++ "\n"

main : IO ()
main = repl "> " showAverage

