module VecSort

import Data.Vect

insertElement : Ord elem => (x : elem) -> (xsSorted : Vect len elem) -> Vect (S len) elem
insertElement x [] = [x]
insertElement x (y :: ys) = case x < y of
                                 False => y :: insertElement x ys 
                                 True => x :: y :: ys

insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insertElement x xsSorted


-- Exercises
my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = S (my_length xs)

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x]

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

my_map2 : (a -> b) -> Vect n a -> Vect n b  -- why doesn't this overload?
my_map2 f [] = []
--my_map2 f (x :: xs) = ?my_map2_rhs_2  -- <leader>o to search and...
my_map2 f (x :: xs) = f x :: my_map2 f xs  -- BOOM!
