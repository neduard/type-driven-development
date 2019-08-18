module Chapter5

import System
import Data.Vect

-- the type IO : (res : Type) -> Type allows us to describe an interaction,
-- encode it in the type and then let the Idris runtime execute it.

main : IO ()
main = do
  putStr "Enter your name: "
  x <- getLine
  putStrLn ("Hello " ++ x ++ "!")

-- Very curious how putStrLn "abc" actually 'returns'
-- io_bind (prim_write "abc\n") (\__bindx => io_pure ()) : IO ()
-- wonder what io_bind, prim_write and io_pure are

printLength : IO ()
printLength = getLine >>= \result => putStrLn (show (length result))

printLength' : IO ()
printLength' = putStr "Input string: " >>= \_ =>
               getLine >>= \input =>
               putStrLn (show (length input))

printLength'' : IO ()
printLength'' = do
  putStr "Input string: "
  input <- getLine
  let len = length input
  putStrLn (show len)

-- exercises
-- ex1
printLonger : IO ()
printLonger = do
  putStr "Enter first string: "
  str1 <- getLine
  putStr "Enter second string: "
  str2 <- getLine
  let longest = if (length str1 > length str2) then str1 else str2
  putStr (longest ++ "\n")


-- ex2
printLonger' : IO ()
printLonger' =
  putStr "Enter first string: " >>=
    \_ => getLine >>=
      \str1 => putStr "Enter second string: " >>=
        \_ => getLine >>=
          \str2 => let longest = if (length str1 > length str2) then str1 else str2 in
                       putStr (longest ++ "\n")

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input) && (length input > 0)
     then pure (Just (cast input))
     else pure Nothing
-- Pure is the same as return in Haskell.  What is the difference?
-- is it that pure is Applicative which is more general than Monad?


readPair : IO (String, String)
readPair = do str1 <- getLine
              str2 <- getLine
              pure (str1, str2)

usePair : IO ()
usePair = do
  (s1, s2) <- readPair
  putStrLn ("You entered " ++ s1 ++ " and " ++ s2 ++ "\n")

readNumbers : IO (Maybe (Nat, Nat))
readNumbers =
  do Just n1 <- readNumber | Nothing => pure Nothing  -- curious syntax.
     Just n2 <- readNumber | Nothing => pure Nothing
     pure (Just (n1, n2))


countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "liftoff!"
countdown time@(S secs) = do putStrLn (show time)
                             usleep (1000*1000)
                             countdown secs
-- exercises
-- ex1
guess : (target : Nat) -> IO ()
guess n = do putStr "Enter a number: "
             Just x <- readNumber | Nothing => guess n  -- "ideally" doesn't mean "has to"
             case compare x n of
                  LT => do putStrLn "Too small"
                           guess n
                  EQ => do putStrLn "Well done!"
                           pure ()
                  GT => do putStrLn "Too big"
                           guess n

-- ex2
rndGuess : IO ()
rndGuess = do
  t <- time
  guess (mod (cast t) 100)


-- ex3
guess' : (target : Nat) -> (guesses : Nat) -> IO ()
guess' n g = do putStr ("Enter guess no. " ++ (show g) ++ ": ")
                Just x <- readNumber | Nothing => do
                  putStrLn "Invalid input"
                  guess' n g
                case compare x n of
                     LT => do putStrLn "Too small"
                              guess' n (g + 1)
                     EQ => do putStrLn "Well done!"
                              pure ()
                     GT => do putStrLn "Too big"
                              guess' n (g + 1)
rndGuess' : IO ()
rndGuess' = do t <- time
               guess' (mod (cast t) 100) 1

-- I wonder if we can make a 'parser' for IO () functions.
-- The idea being if we can automate the random guessing game.

myRepl : String -> (String -> String) -> IO ()
myRepl prompt f = do putStrLn prompt
                     input <- getLine
                     putStrLn (f input)
                     myRepl prompt f

myReplWith : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
myReplWith state prompt f = do
  putStrLn prompt
  input <- getLine
  -- this case statement is ugly.  Any way to replace it with a do notation?
  case f state input of
       Nothing => pure ()
       Just (output, newState) => do putStrLn output
                                     myReplWith newState prompt f

myRepl2 : String -> (String -> String) -> IO ()
myRepl2 prompt f = myReplWith () prompt (\_, inp => Just (f inp, ()))

-- Jump to ReadVect.idr

-- exercises
-- ex1
readToBlank : IO (List String)
readToBlank = do
  line <- getLine
  if line == ""
     then pure []
     else do lines <- readToBlank
             pure (line :: lines)

readFileName : IO String
readFileName = do
  putStr "Enter file name: "
  name <- getLine
  if name == ""
     then do putStrLn "Please enter a filename"
             readFileName
     else pure name

-- ex2
readAndSave : IO ()
readAndSave = do
  putStrLn "Enter text.  Enter a blank line to terminate."
  lines <- readToBlank
  filename <- readFileName
  Right _ <- writeFile filename $ concat (map (++ "\n") lines) |
    Left err => printLn err
  pure ()

-- ex3
-- Below is buggy.  Because fGetLine will return empty on the last line of the file.
--readFileLines : (file : File) -> IO (Maybe (n ** Vect n String))
--readFileLines file = do
--  ended <- fEOF file
--  if ended
--     then pure (Just (_ ** []))
--     else do Right line <- fGetLine file
--             | Left err => do printLn err
--                              pure Nothing
--             maybelines <- readFileLines file
--             case maybelines of
--                  Nothing => pure Nothing
--                  Just (_ ** lines) => pure (Just (_ ** line :: lines))
--

readFileLines : (file : File) -> IO (Maybe (n ** Vect n String))
readFileLines file = do
  Right line <- fGetLine file
  | Left err => do printLn err
                   pure Nothing
  iseof <- fEOF file
  if iseof
     then pure (Just (_ ** []))
     else do maybelines <- readFileLines file
             case maybelines of
                  Nothing => pure Nothing
                  Just (_ ** lines) => pure (Just (_ ** line :: lines))

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right file <- openFile filename Read
  | Left err => do printLn err
                   pure (_ ** [])
  maybelines <- readFileLines file
  case maybelines of
       Nothing => pure (_ ** [])
       Just res => pure res

