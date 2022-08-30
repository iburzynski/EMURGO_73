import Prelude hiding (Maybe, Nothing, Just)

-- *** Algebraic Data Types
-- We can see the "algebraic" nature of types by combining sum and product types into larger types.

-- ABO Group:
data ABOType = A | B | AB | O
-- ABOType is a sum type with four possible values (A + B + AB + O)

-- Rhesus (Rh) Group:
data RhType = Pos | Neg
-- RhType is a sum type with two possible values (Pos + Neg)

data BloodType = BloodType ABOType RhType
--   ^           ^
-- Note: for non-sum types (types with a single Value Constructor), the same name is typically used
-- for both the Type Constructor and Value Constructor.
-- The Type Constructor inhabits the Type level and the Value Constructor inhabits the Value level.
-- These are separate namespaces so there are no name collisions.

-- BloodType is a product type with two fields: a value of type ABOType and a value of type RhType.
-- We can read the data declaration as "A BloodType is an ABOType with an RhType".

aPos :: BloodType
aPos = BloodType A Pos

-- The "algebraic" nature is apparent if we think of types as sets of possible values.
-- If BloodType is a product of two sum types, consisting of 4 and 2 possible values, respectively...
-- how many possible values of our `BloodType` type can be constructed?

-- *** Type Synonyms vs. Custom Types
type FirstName = String
type MiddleName = String
type LastName = String
-- Type synonyms like the examples above do not create new types.
-- They just allow us to use more human-readable names for existing types in our signatures.

-- If we define a function signature like this:
nameTuple :: FirstName -> LastName -> (String, String)
-- The compiler still sees it like this:
-- nameTuple :: String -> String -> (String, String)
nameTuple fn ln = (ln, fn)

fn = "Haskell"
ln = "Curry"
-- There's no way to prevent passing first name and last name arguments in the wrong order:
-- To the compiler they are both just Strings
nt = nameTuple "Curry" "Haskell" -- == ("Haskell", "Curry") << wrong format!

-- Newtype wrappers are used to provide additional type safety in such situations:
newtype FirstName' = FirstName' String
newtype LastName' = LastName' String

-- Note: we could also declare these using record syntax like so:
-- newtype FirstName' = FirstName' {getFirstName :: String}
-- newtype LastName' = LastName' {getLastName :: String}

-- fn' = FirstName' "Haskell"
-- getFirstName fn' => "Haskell"

-- Remember: the `newtype` keyword can only be used for types with a single Value Constructor and a
-- single field. For sum types and product types with multiple fields we must use the `data` keyword.

-- We construct values of our new name types like this:
fn' = FirstName' "Haskell"
--    ^ Value Constructor
--               ^ argument
ln' = LastName' "Curry"
-- LastName' {getLastName = "Curry"}

-- We can now rewrite our `nameTuple` function to use the newtypes and gain additional type safety:
nameTuple' :: FirstName' -> LastName' -> (String, String)
nameTuple' (FirstName' fn) (LastName' ln) = (ln, fn)

nt' = nameTuple' fn' ln' -- << this will typecheck/compile!
-- nt_ = nameTuple' ln' fn' -- << this will not!

-- This is just a very trivial example for demonstration purposes.
-- In a real-world application, the more diligent we are about creating new types to model our data,
-- rather than relying on built-in types and synonyms, the more control we have over the data in our
-- program.

-- If types are defined with record syntax, we can also use the "getter" functions instead of pattern-matching:
-- nameTuple' :: FirstName' -> LastName' -> (String, String)
-- nameTuple' fn ln = (getLastName ln, getFirstName fn)

-- The following example comes from Lesson 16 of Get Programming with Haskell by Will Kurt
data Name = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName

newtype AuthorType = AuthorConstructor Name
newtype Author = Author Name
data Artist = Person Name | Band String
data Creator = AuthorCreator Author | ArtistCreator Artist

-- Record syntax is the preferred way to define product types containing many fields:
data Book = Book {
    author :: Creator
  , isbn :: String
  , bookTitle :: String
  , bookYear :: Int
  , bookPrice :: Double
}
-- It is easier to construct a Book value this way, since the data can be provided in any order:
valis = Book {
    bookTitle = "VALIS"
  , author = AuthorCreator (Author (NameWithMiddle "Philip" "K." "Dick"))
  , bookYear = 1981
  , bookPrice = 12.99
  , isbn = "0547572417"}

-- With regular product syntax we would have to define it like this:
data Book' = Book' Creator String String Int Double
-- And to construct a value we need to keep track of the order of the fields:
valis' = Book' (AuthorCreator (Author (NameWithMiddle "Philip" "K." "Dick"))) "0547572417" "VALIS" 1981 12.99

data VinylRecord = VinylRecord {
    artist :: Creator
  , recordTitle :: String
  , recordYear :: Int
  , recordPrice :: Double
}

-- Note how we need to declare distinct names for the common fields in `Book` and `VinylRecord`.
-- Field names are defined as functions in the global namespace and thus can't be reused.
-- This is one of Haskell's shortcomings that must be worked around.
-- One solution is to encapsulate the data declarations in their own modules:
-- then we can use qualified imports to prevent naming conflicts:

-- ex. import qualified Book as B
--     import qualified VinylRecord as VR

-- We could now reuse "title" as a field name for both Book and VinylRecord, and call them like this:
-- B.title
-- VR.title

-- *** Parameterized Types (see Lesson 18 in Get Programming with Haskell):
-- Types can have parameters (i.e. take arguments) just like functions
-- Type parameters are always *other types*
-- Type parameters are represented by type variables (a, b, c, etc.)
-- Parameterized types allow us to define generic data structures that work on a variety of data
-- This is called "parametric polymorphism" in Haskell; it's similar to "generics" in OOP languages

-- Just like in function definitions, a variable appears on both the left and right side of the =.
-- The variable to the left of the equals is the binding: it assigns the name to the variable.
-- Wherever the variable appears on the right is where we're using what we bound on the left.

-- The built-in `Maybe` type is a commonly used parameterized type. It is defined like this:
data Maybe a = Nothing | Just a -- a Maybe value is either `Nothing` (null value) or a `Just` value containing some data of any type
--         ^                  ^
--         variable binding   variable use
  deriving Show
-- for more on the Maybe type, see this link: https://typeclasses.com/beginner-crash-course/basic-types-2#maybe
-- and Lesson 19 of Get Programming with Haskell.

sampleMaybe :: Maybe Book
sampleMaybe = Just valis

sampleMaybe' :: Maybe Bool
sampleMaybe' = Nothing

-- *** Polymorphism in Haskell: Parametric vs. Ad-hoc
-- Parametric polymorphism is when we have a single abstract function that behaves the same way
-- regardless of which type of data it is applied to.
-- Here we see an unconstrained type variable `a` representing data of any type, since `safeHead`
-- behaves the same way on every list regardless of the data contained inside.
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
-- More on parametric polymorphism: https://en.wikipedia.org/wiki/Parametric_polymorphism

-- When we add a typeclass constraint to the type variable, it is called "ad-hoc polymorphism":
-- Here `a` represents any type that is an instance of the `Integral` type class (Int, Integer, Word)
-- The polymorphism of the function is constrained to a limited range of types. Ad-hoc polymorphism
-- is also called "constrained polymorphism" for this reason.
safeDiv :: Integral a => a -> a -> Maybe a
safeDiv x 0 = Nothing
safeDiv x y = Just (x `div` y)
-- Ad-hoc polymorphism is related to "operator overloading": think about how '+' can be used in some
-- languages to add numbers of varying types as well as concatenate strings.
-- The same operation has multiple implementations depending on the types of its arguments.
-- `safeDiv` works on all instances of the `Integral` type, which each have their own implementation
-- of the `div` function. Both `safeDiv` and `div` can thus be thought of as "overloaded" functions.
-- More on ad-hoc polymorphism: https://en.wikipedia.org/wiki/Ad_hoc_polymorphism


-- We can define a custom single-parameter pair type, containing two values of the same type:
data HomogenousPair a = HomogenousPair a a
-- Our `HomogenousPair` *type constructor* has a single parameter `a`: the type of data it contains
-- Our `HomogenousPair` *value constructor* has two parameters, representing two values of the same type
-- The type variable `a` in the *type constructor* is a placeholder for a concrete type
-- The same variable `a` in the *value constructor* is a placeholder for two values of that type

-- We can construct a HomogenousPair value like this:
hp = HomogenousPair "hello" "world"
-- But this will not typecheck, because the values are not of the same type:
-- hp' = HomogenousPair "hello" 123

-- We can define a custom type containing data of as many different types as we like, represented by
-- multiple type variables (here `a` and `b`):
data Pair a b = Pair a b
-- `a` and `b` are both placeholders for concrete types, and can be of the same type or different

p = Pair 1 "hello"

data Triple a b c = Triple a b c

-- *** Kinds:
-- The kind of a type indicates its *arity* (number of type parameters)
-- Kinds can be thought of as "types of types"
-- Just as every value has a type signature in Haskell, every type has a *kind signature*
-- Kind signatures are expressed using asterisks to represent the type parameters, separated by arrows
-- Types with no parameters have kind *
-- One parameter: * -> *
-- Two parameters: * -> * -> *
-- Type `:kind` or `:k` in GHCi followed by a Type (i.e. `Int`) or Type Constructor (i.e. `Maybe`)
--   to see its kind signature

-- Questions:
-- What is the kind of `Pair`?
-- What is the kind of `HomogenousPair`?
-- What is the kind of `Triple`?
-- What is the kind of `Maybe`?
-- What is the kind of `Maybe Int`?