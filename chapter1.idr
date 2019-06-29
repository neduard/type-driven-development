-- Idea: use types as first class language construct: i.e. allow them to be passed as arguments,
-- returned from functions and assigned to variables.
-- Idea first-class language constructs:
-- let's start with C:
-- numbers are first-class
-- pointers are first-class
-- arrays are not (can not be returned or assigned)
-- functions are not (you have function pointers but you can not have a 
--                    function that returns a function)
-- Python:
-- numbers, lists, functions, but not types.

-- Types are used at multiple levels:
-- in assembley they represent how a register should be interpreted
--   (signed, unsigned, 16, 32 bit, etc, float, double, etc)
-- at compiler level, how different parts of the program interpret data at runtime
--   (how many bytes to push to the stash for another function to act on them)
-- by the programmer: to abstract away various concepts that they use when
--   designing the program (humans don't think in numbers, they think in
--                          abstractions)

-- What is type driven development: use types as a "plan".  In the same way as we
-- use UML diagrams, use types to specify the input and output of functions.

-- Type, define refine loop:
--  start with generic type (say Matrix), define your adder function,
--  notice invalid cases => refine your type.

-- Exercise
-- Vect n elem -> Vect n elem
--  reverse function
-- Vect n elem -> Vect (n * 2) elem
--  duplicate each element?
-- Vect (1 + n) elem -> Vect n elem
--  remove an element
-- Note: this is something we can not do in most other langauges (o.l.):
--  specify Vect (1 + n) elem as a type
-- Bounded n, Vect n elem -> elem
--  get the nth element.
-- Note: this starts illustrating the power of dependent types:
--  Bounded n is a property, meaning it is a type that poves that
--  the number is between 0 and n-1.  We can not express this in o.l.

-- Why pure functions?  Super easy to debug and to reason about:
-- output based only on inputs.

-- Environment setup: vim-idris
-- to open idris-response buffer, <LocalLeader>i
-- if closed, just :b idris-response
-- bindings:
-- t: type
-- e: eval window
-- r: reload and typecheck
-- h: help
--
-- d: define
-- c: case split

module Main

main : IO ()
main = putStrLn "Hello, Idris World!"

-- Holes
main2 : IO ()
main2 = putStrLn ?greeting  -- position cursor on ?greeting and ask for type.

main3 : IO ()
--main3 = putStrLn 'x'  --type error
--main3 = putStrLn (?convert 'x')
main3 = putStrLn (cast 'x')

-- First class types
StringOrInt : Bool -> Type  -- leader_d for define
--StringOfInt x = ?StringOfInt_rhs  -- leader_c for case split
StringOrInt False = String
StringOrInt True = Int  -- leader_c for case split

getStringOrInt : (x : Bool) -> StringOrInt x  -- here we go :)
getStringOrInt False = "Ninety four"
getStringOrInt True = 94


valToString : (x : Bool) -> StringOrInt x -> String
valToString x y = case x of
                       True => cast val
                       False => val


