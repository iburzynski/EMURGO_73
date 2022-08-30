{-# LANGUAGE InstanceSigs #-}
import           Data.Foldable (foldl')
import           Data.List     (sort)

-- *** Parameterized & Recursive Types
-- We can reimplement the built-in list type to further understand polymorphic/parametrized types
-- and also demonstrate how types can be recursive:

data List a = Empty | Cons a (List a)
  deriving (Eq, Read)
-- data [] a = []   |      a : [] a
--        ^ List is a parameterized type that can contain data of any one type
--             ^ A List is either an Empty list...
--                         ^ or a value of type `a` Cons'ed to another List of `a`'s

-- the value constructor `Cons` is a function that:
--   * takes a value of type `a` and a value of type `List a`
--   * returns a value of type `List a`
empty = Empty
hey = Cons 'h' (Cons 'e' (Cons 'y' Empty))
--         'h' : ('e' : ('y' : []))

-- *** Typeclasses:
-- Allow us to define polymorphic functions with different implementations for different types
-- This is called "ad hoc polymorphism" and is related to "overloading" of functions
-- A typeclass is:
--   * *not* like classes in Object-Oriented languages
--   * *not* a type, but a *category of types* whose instances are types (not values)
--   * an abstract interface containing a minimal set of methods
-- These methods must be implemented for a type in order for it to become an instance of that class
-- Implementing the minimal methods typically implements additional derivative methods for free
--   * i.e. implementing (==) for a type to make it an instance of Eq automatically implements (/=)
-- We can make our types instances of existing typeclasses or define our own via the `class` keyword
-- Instance definitions for some typeclasses are simple enough for GHC to implement on its own
-- The following typeclasses can be instantiated automatically using the *deriving* keyword:
--   * Show: convert values of a type to a string (show)
--   * Read: convert a string to a value of a type (use discouraged - use parsers instead) (read)
--   * Eq: check two values of a type for equality (==, /=)
--   * Ord: compare two values of a type
--   * Enum: enumerate the values of a type
--   * Bounded: define upper and lower bounds for a type

instance Show a => Show (List a) where
  show :: Show a => List a -> String
  show xs = showList True xs
    where
      showList :: Show a => Bool -> List a -> String
      showList True   Empty         = "[]"
      showList True  (Cons x Empty) = "[" ++ show x ++ "]"
--                                           ^ the `show` applied here is the one defined in the
--                                             `Show` instance for whatever type our List contains
      showList True  (Cons x xs)    = "[" ++ show x ++ ", " ++ showList False xs
      showList False (Cons x Empty) = show x ++ "]"
      showList False (Cons x xs)    = show x ++ ", " ++ showList False xs


myList = Cons 1 (Cons 2 (Cons 3 Empty))
-- [1, 2, 3]

-- *** Derived & Manual Instances
type FirstName = String
type LastName  = String

newtype Name = Name (FirstName, LastName)
  deriving (Show, Eq)

-- instance Eq Name where
--    (==) (Name (f1, l1)) (Name (f2, l2)) = f1 == f2 && l1 == l2

-- What happens if we derive an Ord instance for Name?
names = sort [ Name ("Philip", "Wadler")
             , Name ("Michael", "Peyton Jones")
             , Name ("Charles", "Hoskinson")
             , Name ("Jeremy", "Wood")]

-- To sort Names by last name first, we can define our own Ord instance implementing `compare`:
instance Ord Name where
  compare :: Name -> Name -> Ordering
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)
  --                                      = compare (f1, l1) (f2, l2)

-- Ordering: GT | LT | EQ

data D6 = S1 | S2 | S3 | S4 | S5 | S6
  deriving (Show, Eq, Ord, Bounded)

-- The derived Eq, Ord, and Bounded instances all behave the way we'd like them to.
-- But the Enum instance gives us the following behavior:
-- fromEnum S1 => 0
-- because the derived instance enumerates the value constructors beginning with 0.
instance Enum D6 where
  fromEnum :: D6 -> Int
  fromEnum S1 = 1
  fromEnum S2 = 2
  fromEnum S3 = 3
  fromEnum S4 = 4
  fromEnum S5 = 5
  fromEnum S6 = 6

  toEnum :: Int -> D6
  toEnum 1 = S1
  toEnum 2 = S2
  toEnum 3 = S3
  toEnum 4 = S4
  toEnum 5 = S5
  toEnum 6 = S6
  toEnum _ = error "Number out of range"

enumEx :: [D6]
enumEx = map toEnum [1, 3, 4, 5, 6, 2, 1]

-- *** Example: Scalar & Vector Addition
newtype Scalar a = Scalar { getScalar :: a }
newtype Vector a = Vector { getVector :: [a] }

-- We can create our own `Addable` typeclass using a `class` declaration:
class Addable a where
  -- A class declaration contains a set of method names with accompanying type signatures:
  (+++) :: a -> a -> a
  -- "The `+++` operator takes two values of type `a` and returns a value of type `a`"

-- Now we define `Addable` instances for our Scalar and Vector types:
instance Num a => Addable (Scalar a) where
--       ^ we need a Num constraint on the type inside the Scalars since we need to call (+) on them
--       (+) :: Num a => a -> a -> a
  Scalar x +++ Scalar y = Scalar (x + y)

instance Num a => Addable (Vector a) where
  Vector xs +++ Vector ys = Vector $ zipWith (+) xs ys

-- zipWith (\x y -> ...) [1, 2, 3] ['a', 'b', 'c', 'd'] => ["1a", "2b", "3c"]

newtype Matrix a = Matrix { getMatrix :: [[a]] } deriving Show

instance Num a => Addable (Matrix a) where
  Matrix xss +++ Matrix yss = Matrix $ addMatrix xss yss
    where
      addMatrix [] yss            = yss
      addMatrix xss []            = xss
      addMatrix (xs:xss) (ys:yss) = zipWith (+) xs ys : addMatrix xss yss

m1 = Matrix [ [1, 2, 3]
            , [4, 5, 6] ]

m2 = Matrix [ [7, 8, 9]
            , [0, 1, 2] ]

-- *** Exercise: Folding our Custom List ***
-- 1. Define a Foldable instance for our custom List type, so we can fold over it like we can with
--   built-in lists.
-- 2. Then define a version of the `reverse` function for our custom List using a fold.

myReverse :: List a -> List a
myReverse = undefined
