module Chapter4
import Data.Vect

data Bool = False | True

-- Enumeration types.  Literally an enum from C.
{-
From wiki:
> An enumerated type can be seen as a degenerate case: a tagged union of unit
> types. It corresponds to a set of nullary constructors and may be implemented
> as a simple tag variable, since it holds no additional data besides the value
> of the tag.
-}
data Direction = North
               | East
               | South
               | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

-- Union types.  Why are they called union?  This is different
-- from a union in C.
-- From above, we see that this is basically a union of 'product types'.
{-
From wiki:
> The expression of an instance of a product type will be a tuple, and
> is called a "tuple type" of expression. A product of types is a direct
> product of two or more types.
In other words it is an union of structs (which it is).
-}
-- With documentation.  Note that we first have the | separator
-- and right after we have the ||| for documentation.
data Shape = ||| Triangle with base and height
             Triangle Double Double
           | ||| Rectangle with width and height
             Rectangle Double Double
           | ||| Circle with radius.
             Circle Double

area : Shape -> Double
area (Triangle base height) = (base * height) / 2
area (Rectangle width height) = width * height
area (Circle radius) = pi * radius * radius

data Shape2 : Type where
  -- need to use 2 suffix else we get a conflict with above.
  Triangle2 : Double -> Double -> Shape2
  Rectangle2 : Double -> Double -> Shape2
  Circle2 : Double -> Shape2

-- Recursive types
data MyNat = MyZ | MyS MyNat  -- Peano arithmetic <3


data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

testPicture : Picture
testPicture = Combine
                (Translate 5 5 (Primitive (Rectangle 20 10)))
                (Combine (Translate 35 5 (Primitive (Circle 5)))
                         (Translate 15 25 (Primitive (Triangle 10 10))))

pictureArea : Picture -> Double
pictureArea (Primitive s) = area s
pictureArea (Combine p1 p2) = pictureArea p1 + pictureArea p2
pictureArea (Rotate r p) = pictureArea p  -- pictureArea ?h  will be autocompleted
pictureArea (Translate x y p) = pictureArea p

data Infinite = Forever Infinite  -- this typechecks though!  why?


-- Generic data types
-- These are "templates" in C++ world.  They don't exist in C land, except
-- for maybe arrays/pointers of type T.

data Biggest = NoTriangle | Size Double  -- really this is maybe.

biggestTriangle : Picture -> Biggest
-- would have been nice to introduce the @ syntax before.  I knew it from Haskell.
biggestTriangle (Primitive (t@(Triangle x y))) = Size (area t)
biggestTriangle (Primitive (Rectangle x y)) = NoTriangle
biggestTriangle (Primitive (Circle x)) = NoTriangle
-- Holly carp that is manual!
-- need to use >>=
biggestTriangle (Combine x y) = case biggestTriangle x of
                                     NoTriangle => case biggestY of
                                                        NoTriangle => NoTriangle
                                                        Size size2 => Size size2
                                     Size size => case biggestY of
                                                       NoTriangle => Size size
                                                       Size size2 => Size (max size size2)
                                where 
                                    biggestY : Biggest
                                    biggestY = biggestTriangle y
biggestTriangle (Rotate r p) = biggestTriangle p
biggestTriangle (Translate x y p) = biggestTriangle p

--data Maybe type = Nothing | Just type

safeDivide : Double -> Double -> Maybe Double
safeDivide x y = if y /= 0 then Just (x / y)
                           else Nothing

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)
%name Tree tree, tree1

insert : Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x orig@(Node left y right) = case compare x y of
                                    LT => Node (insert x left) y right
                                    EQ => orig  -- now we introduce @ patterns
                                    GT => Node left y (insert x right)


data BSTree : Type -> Type where
  BEmpty : Ord a => BSTree a
  BNode  : Ord a => (left : BSTree a) -> (val : a) -> (right : BSTree a) -> BSTree a

insert2 : a -> BSTree a -> BSTree a
insert2 x BEmpty = BNode BEmpty x BEmpty
insert2 x orig@(BNode left val right) = case compare x val of
                                             LT => BNode (insert2 x left) val right
                                             EQ => orig
                                             GT => BNode left val (insert2 x right)

-- Exercises!
-- Ex1
listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

-- Ex2
treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = treeToList left
                                 ++
                                 x :: treeToList right

-- Ex3 (classic)
data Expr : Type where
  Val : Int -> Expr
  Add : (t1 : Expr) -> (t2 : Expr) -> Expr
  Sub : (s1 : Expr) -> (s2 : Expr) -> Expr
  Mul : (f1 : Expr) -> (f2 : Expr) -> Expr


evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add t1 t2) = evaluate t1 + evaluate t2
evaluate (Sub s1 s2) = evaluate s1 - evaluate s2
evaluate (Mul f1 f2) = evaluate f1 * evaluate f2

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = Just (max x y)

biggestTriangle2 : Picture -> Maybe Double
biggestTriangle2 (Primitive t@(Triangle x y)) = Just (area t)
biggestTriangle2 (Primitive (Rectangle x y)) = Nothing
biggestTriangle2 (Primitive (Circle x)) = Nothing
biggestTriangle2 (Combine p1 p2) = maxMaybe (biggestTriangle2 p1) (biggestTriangle2 p2)
biggestTriangle2 (Rotate r p) = biggestTriangle2 p
biggestTriangle2 (Translate x y p) = biggestTriangle2 p

-- Dependent types.  Woot!
-- Redefined in ex1
--data PowerSource = Petrol | Pedal
                
-- How is this type fundamentally different from Maybe a?
--data Vehicle : PowerSource -> Type where
--  Bicycle : Vehicle Pedal
--  Car     : (fuel : Nat) -> Vehicle Petrol
--  Bus     : (fuel : Nat) -> Vehicle Petrol
--
--wheels : Vehicle power -> Nat
--wheels Bicycle = 2
--wheels (Car fuel) = 4
--wheels (Bus fuel) = 4
--
--refuel : Vehicle Petrol -> Vehicle Petrol
--refuel (Car fuel) = Car (fuel + 20)
--refuel (Bus fuel) = Car (fuel + 10)
--refuel Bicycle impossible

data MyWeirdType : Type -> Type where
  A : MyWeirdType (Maybe Int)
  B : Nat -> MyWeirdType Double
  C : Nat -> MyWeirdType (Maybe Double)

myWeirdFunction1 : MyWeirdType Double
myWeirdFunction1 = B 5

myWeirdFunction2 : a -> MyWeirdType a
myWeirdFunction2 x = ?myWeirdFunction2_rhs  -- can this be defined in any way?

-- See Vect.idr

-- Ex1,2
data PowerSource = Pedal | Petrol | Electric
data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Unicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  PetrolCar : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Tram : Vehicle Electric
  ElectricCar : Vehicle Electric
-- Feels like a slightly contrived example...
-- there is power in it in terms of how depending on the PowerSource
-- Vehicle has a different definition (i.e. in terms of fuel).


wheels : Vehicle p -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Motorcycle fuel) = 2
wheels (PetrolCar fuel) = 4
wheels (Bus fuel) = 4

-- Ex 3,4
vectTake : (k : Fin n) -> Vect n a -> Vect (finToNat k) a
vectTake FZ xs = []
--vectTake (FS k') xs = ?vectTake_rhs_2  -- case on xs (it can't be [])
--vectTake (FS k') (x :: xs) = ?vectTake_rhs_1  -- search on ?rhs
vectTake (FS k') (x :: xs) = x :: vectTake k' xs  -- BAM programs that write themselves.

-- Remember, types are proofs so you can freely substitute 'proof'
-- with the word type
data LT : Nat -> Nat -> Type where
  LTZero : Chapter4.LT Z (S n)
  LTSucc : Chapter4.LT a b -> Chapter4.LT a (S b)

-- I like this version since it clearly shows what we need.
-- It is not as easy to workwith though.
vectTake' : LTE k n -> Vect n a -> Vect k a
vectTake' LTEZero xs = []
vectTake' (LTESucc k') (x :: xs) = x :: vectTake' k' xs

-- Ex5
sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = map (\i => (Data.Vect.index i xs) + (Data.Vect.index i ys))
                               (integerToFin pos n)

-- Now go to DataStore.idr
