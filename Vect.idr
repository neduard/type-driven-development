module Vect
-- File referenced from Chapter4.idr

data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : (x : a) -> (xs : Vect len a) -> Vect (S len) a

%name Vect xs, ys, zs

append : Vect n a -> Vect m a -> Vect (n + m) a
append [] ys = ys
append (x :: xs) ys = x :: append xs ys  -- <3<3<3 autocompleted

zip : Vect n a -> Vect n b -> Vect n (a,b)
zip [] [] = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys  -- <3<3<3<3 autocompleted

-- Fin n means a number that has an upper bound of n.
-- i.e. proof that the element < n
-- FZ : Fin n, for all n 0 < S n
-- FS (FZ) : proof that 1 < S (S n)
--   therefore FS : Fin n -> Fin (S n)
data Fin : Nat -> Type where
  FZ : Fin (S n)
  FS : Fin n -> Fin (S n)

index : Fin n -> Vect n a -> a
index FZ (x :: xs) = x
index (FS k) (x :: xs) = index k xs

natToFin : Nat -> (n : Nat) ->  Maybe (Fin n)
natToFin k Z = Nothing  -- Nothing is < 0
natToFin Z (S j) = Just FZ  -- Z < S n
natToFin (S k) (S j) = map FS (natToFin k j)  -- Functor example

integerToFin : Integer -> (n : Nat) -> Maybe (Fin n)
integerToFin x Z = Nothing
integerToFin x (S k) = if x == 0 then Just FZ
                                 else map FS (integerToFin (x-1) k)  -- <3 Functors
-- Note to self: functor has map function and it's type is (a->b) -> f a -> f b
-- there is bit of missmatch between haskell: in hl map is of type (a->b) -> [a] -> [b]
-- fmap is the more general case.
-- If we look at https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#line-966
-- we see fmap for [] is defined as map (so map is the primitive operation).


-- In the book we now go to TryIndex.idr where we import Data.Vect.
-- Given that we've already implemented integerToFin here, I'll just
-- continue in this file.
tryIndex : Integer -> Vect n a -> Maybe a
-- we want to go from x to Maybe Fin n to Maybe a
tryIndex {n} x xs = (map index (integerToFin x n)) <*> (Just xs)  -- Woot!
-- applicative takes a f (a->b) -> f a -> f b

tryIndex' : Integer -> Vect n a -> Maybe a
tryIndex' {n} x xs = map (flip index xs) (integerToFin x n) -- Woot2!


-- Return to Chapter4.idr
