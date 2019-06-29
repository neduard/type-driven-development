module Main

average : (str : String) -> Double
average str = let numWords = length (words str)
                  totalLength = sum (lengths (words str)) in ((cast totalLength) / (cast numWords))
  where lengths : List String -> List Nat
        lengths xs = map length xs

main : IO ()
main = repl "Enter a string: " showAverage
  where showAverage : String -> String
        showAverage s = "Avg word length: "
                        ++ (show (average s))
                        ++ "\n"

-- using 'the' to explicitly annotate the type of things:
-- note this NOT cast.  `the` has the type (t : Type) -> (value : t) -> t
-- the Double 4 gives 4.0

integerval : Integer
integerval = 4

doubleval : Double
doubleval = 0.1

intsumval : Integer
--intsumval = integerval + (the Integer doubleval)  -- this still type errors
intsumval = integerval + (cast doubleval)  -- OK

-- type of cast is (Cast from to) => from -> to  -- notice the => meaning there needs
-- to be a definition of Cast from to to be able to convert from from to to :)

-- idris has eager evaluation but also has Lazy type to support Lazy evaluations.
-- in idris we need to annotate function as we can not do type inference on them
-- (as opposed to Haskell)                                                         

identity : ty -> ty  -- here, ty is an actual VARIABLE (type variable)
identity x = x

mythe : (ty : Type) -> ty -> ty
mythe t x = x  -- this is fine, the t here is the same as ty in the type definition

-- generic types (remember from Cast from to)
double : Num ty => ty -> ty  -- Num ty can also be seen as a type "constraint" or *Interface*
double x = x + x

-- Higher order functions
twice : (a -> a) -> a -> a
twice f x = f (f x)

Shape : Type
rotate : Shape -> Shape
-- I like how we don't have to define rotate

turn_around : Shape -> Shape
turn_around s = twice rotate s

turn_around' : Shape -> Shape
turn_around' = twice rotate  -- 'point-free' notation


-- Anon functions
myanonexample : Integer -> Integer
myanonexample = \x => x * x  -- note the => instead of -> (as in Haskell)

unit_type_example : ()  -- unit TYPE
unit_type_example = ()  -- unit VALUE  (overloaded syntax)

-- multi typles are nested pairs (which seems kind of awkward if you think about it)
-- (1,2,3) == (1, (2, 3))  -- note there is no unit type

-- Note: Idris does have type inference in the REPL.  You can type [1,2,3] and
-- it will know it is List Integer


-- See Average.idr

-- Whitespace rule:
-- same column, new definition
-- indented: continuation of previous expression


-- Documentation comments:
||| This is a documentation comment for myAwesomeFunction
||| (which is not very awesome)
||| @arg an awesome and totally relevant argument
myAwesomeFunction : arg -> ()
myAwesomeFunction x = ()  -- note that arg in documentation is one in type defintion

-- see AveMain.idr

{-
Exercises:
1: (String, String, String)
   List String
   ((Char, String), String)  -- nesting normally occures on rhs
-}
-- ex 2
palindrome : String -> Bool
palindrome s = s == (reverse s)

-- ex 3
ipalindrome : String -> Bool
ipalindrome s = let lower = toLower s in
                    palindrome lower


-- ex 4
palindrome10 : String -> Bool
palindrome10 s = ((length s) > 10) && (palindrome s)

-- ex 5
palindromen : Nat -> String -> Bool
palindromen k s = ((length s) > k) && (palindrome s)

-- ex 6
counts : String -> (Nat, Nat)
counts s = (length (words s), length s)

-- ex 7
top_ten : Ord a => List a -> List a
top_ten = List.take 10 . reverse . sort   -- point-free notation ftw

-- ex 8
over_length : Nat -> List String -> Nat
over_length k xs = length (filter (> k) (map length xs))

-- bonus:
over_length' : Nat -> List String -> Nat
over_length' k = length . filter (> k) . map length  -- point-free version

-- ex 9
-- too lazy
generic_repl : Show a => (String -> a) -> IO ()
generic_repl f = repl "Enter a string " ((++ "\n") . show . f)

