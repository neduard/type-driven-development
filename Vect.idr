module Vect

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

-- TODO
---- Proof that the Nat <= Fin n
---- FZ : Fin n
---- FS : Fin (S n)
--data Fin : Nat -> Type where
--  FZ : Fin n
--  FS : Fin (S n) -> Fin n
--
--index : Fin (S n) -> Vect (S n) a -> a
--index FZ (x :: xs) = x
--index (FS x) (y :: ys) = index x xs

