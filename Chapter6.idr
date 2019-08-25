module Chapter6

import Data.Vect

Position : Type
Position = (Double, Double)

-- this showcases that a type signature is really an expression.
valToString : (isInt : Bool) -> (case isInt of
                                      False => String
                                      True => Int) -> String
valToString False x = trim x
valToString True x = cast x

AdderType : (num_args : Nat) -> Type
AdderType Z = Int
AdderType (S k) = Int -> AdderType k   -- Mind Blown!
                                       -- what exactly is -> ?
                                       -- I can't do :t (->) on it..
                                       -- I can do :t Int -> Nat

-- Seems like we require an accumulator...
--adder : (num_args : Nat) -> AdderType num_args
--adder Z = 0
--adder (S k) = \x => ?h

adder : (num_args : Nat) -> (acc : Int) -> AdderType num_args
adder Z acc = acc
adder (S k) acc = \x => adder k (acc + x)

AdderType2 : (num_args : Nat) -> Type -> Type
AdderType2 Z x = x
AdderType2 (S k) x = x -> AdderType2 k x

adder2 : Num t => (num_args : Nat) -> t -> AdderType2 num_args t
adder2 Z acc = acc
adder2 (S k) acc = \x => adder2 k (x + acc)


-- Let's go to type-checked Printf.idr!

--exercises
--ex1

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Double)

testMatrix : Matrix 2 3
testMatrix = [[0,0,0], [0,0,0]]

--ex2 (see Printf.idr)


--ex3
TupleVect : Nat -> Type -> Type
TupleVect Z x = ()
TupleVect (S k) x = (x, TupleVect k x)

test : TupleVect 4 Nat
test = (1,2,3,4,())


-- Go to DataStore.idr
