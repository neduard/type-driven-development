module ReadVect

import Data.Vect

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure (x :: xs)  -- nice..

---- why do we have to define the length??
--data VectUnknown : Type -> Type where
--     MkVect : (len : Nat) -> Vect len a -> VectUnknown a
--
--readVect : IO (VectUnknown String)
--readVect = do x <- getLine
--              if x == "" then pure (MkVect 0 [])
--                         else do MkVect len xs <- readVect
--                                 pure (MkVect (S len) (x :: xs))

data VectUnknown : Type -> Type where
  MkVect : Vect len a -> VectUnknown a

readVect' : IO (VectUnknown String)
readVect' = do
  x <- getLine
  if x == ""
     then pure (MkVect [])
     else do
       MkVect xs <- readVect'
       pure (MkVect (x :: xs))


readVect : IO (n ** Vect n String)  -- Oooh, interesting!
readVect = do
  x <- getLine
  if x == ""
     then pure (_ ** [])
     else do
       (_ ** xs) <- readVect
       pure (_ ** x :: xs)

-- This is fairly ugly.  Do we have (a : Nat) -> (b : Nat) -> Maybe (a with (_)
  -- This is fairly ugly.  Do we have (a : Nat) -> (b : Nat) -> Maybe (a | with_pat = ?This_rhs
myExactLength : (l : Nat) -> (Vect len a) -> Maybe (Vect l a)
myExactLength Z [] = Just []
myExactLength Z (x :: xs) = Nothing
myExactLength (S k) [] = Nothing
myExactLength (S k) (x :: xs) = case myExactLength k xs of
                                     Nothing => Nothing
                                     Just ys => Just (x :: ys)

zipInputs : IO ()
zipInputs = do
  putStrLn "Enter first vector (blank line to end): "
  (l1 ** v1) <- readVect
  putStrLn "Enter second vector (blank line to end): "
  (l2 ** v2) <- readVect
  --(Just v2l1) <- exactLength l1 v2 | Nothing putStrLn "Vectors have different lengths."
  case myExactLength l1 v2 of  -- how DO we implement exact length?
       Nothing => putStrLn "Vectors have different lengths."
       Just v2l1 => putStrLn (show (zip v1 v2l1))

-- Go back to Chapter5.idr
