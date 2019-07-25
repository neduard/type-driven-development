module Chapter3

import Data.Vect

-- In this chapter we do interactive development using the
-- "type, define, refine" loop

allLengths : List String -> List Nat  -- <leader>_d
--allLengths xs = ?allLengths_rhs       -- <leader>_c over xs
allLengths [] = []
--allLengths (word :: words) = ?allLengths_rhs_2       -- <leader>_c
--allLengths (word :: words) = length word :: ?rest
-- <l>_e allLengths
-- ["abc", "cde"] will give partial evaluation
allLengths (word :: words) = length word :: allLengths words


xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)

mutual
  isEven' : Nat -> Bool
  isEven' Z = True
  isEven' (S k) = isOdd' k

  isOdd' : Nat -> Bool
  isOdd' Z = False
  isOdd' (S k) = isEven' k

fourInts : Vect 4 Int
fourInts = [1,2,3,4]

-- [1,2] is syntactic sugar to ANY datatype with :: and Nil constructors
-- the (Vect _ _) [1,2,3]
-- the (List _) [1,2]
total allLengths2 : Vect n String -> Vect n Nat  -- note we can annotate 'total'
-- (it is still attempted to be inferred by Idris)
allLengths2 [] = []
allLengths2 (x :: xs) = length x :: allLengths2 xs

allLengths3 : Vect n String -> Vect n Nat
allLengths3 [] = []
allLengths3 (word :: words) = length word :: allLengths3 words

-- See VecSort.idr

--createEmpties : Vect n (Vect 0 elem)
--createEmpties = replicate _ []  -- forced by the type to have n elements.
--
--transposeHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect len elem)) -> Vect n (Vect (S len) elem)
--transposeHelper [] [] = []
--transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys
--
--transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
--transposeMat [] = createEmpties
--transposeMat (x :: xs) = let xsTrans = transposeMat xs in
--                             transposeHelper x xsTrans


--transposeHelper : Vect m elem -> Vect m (Vect len elem) -> Vect m (Vect (S len) elem)
--transposeHelper [] [] = []
--transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys
--
--transposeMat : Vect n (Vect m elem) -> Vect m (Vect n elem)
--transposeMat [] = replicate _ []
--transposeMat (x :: xs) = let xsTrans = transposeMat xs in
--                             transposeHelper x xsTrans

-- Exercises
-- ex1
transposeMat : Vect n (Vect m a) -> Vect m (Vect n a)
transposeMat [] = replicate _ []
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
--                           zipWith ?f x xsTrans  -- Did I say how much I love this language?
                             zipWith (::) x xsTrans

transposeMat2 : Vect n (Vect m a) -> Vect m (Vect n a)
transposeMat2 [] = replicate _ []
transposeMat2 (x :: xs) = zipWith (::) x (transposeMat2 xs)

--ex2
addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

addMatrix2 : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix2 = zipWith (zipWith (+))  -- <3

--ex3 (without using transpose) 
-- instead we rely on the observation that the result matrix's
-- first row is equal to first row of m1 times m2; hence
-- m1 x m2 = map (rowMult m2) m1
multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix m1 m2 = map (flip rowMult m2) m1
  where
    rowMult : Num a => Vect m a -> Vect m (Vect p a) -> Vect p a
    rowMult xs ms = foldr (zipWith (+)) (replicate _ 0) $ zipWith (\x,r => map (*x) r) xs ms

-- Type-level variables
-- Note how we were already were passing elem as an implicit argument.
myAppend : (elem : Type) -> (n : Nat) -> (m : Nat) -> Vect n elem -> Vect m elem -> Vect (n + m) elem
myAppend elem Z m [] ys = ys
myAppend elem (S len) m (x :: xs) ys = x :: myAppend elem len m xs ys

myLength : Vect n a -> Nat
myLength {n} xs = n  -- So much nicer than haskell!

createEmpties2 : Vect n (Vect 0 a)
createEmpties2 {n} = replicate n []

createEmpties3 : Vect n (Vect 0 a)
createEmpties3 {n = Z} = []
createEmpties3 {n = (S k)} = [] :: createEmpties3 {n = k}

createEmpties4 : Vect n (Vect 0 a)
createEmpties4 {n = Z} = []
createEmpties4 {n = (S k)} = [] :: createEmpties4  -- Wooot

