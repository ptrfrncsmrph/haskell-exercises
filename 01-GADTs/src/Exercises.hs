{-# LANGUAGE GADTs, LambdaCase, InstanceSigs #-}
module Exercises where

import Data.Function as Function
import Data.Foldable



{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  Nil :: CountableList
  Cons :: Countable a => a -> CountableList -> CountableList

-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList = \case
  Nil -> 0
  Cons c list -> count c + countList list

-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero = \case
  Nil -> Nil
  Cons c list -> case count c of
    0 -> list
    _ -> Cons c (dropZero list)

-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts = \case
  Nil -> Nil
  Cons c list -> error "Don't know if `c` is an Int"




{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  AnyNil :: AnyList
  AnyCons :: a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList =
  Function.fix (\f acc ->
    \case
      AnyNil -> acc
      AnyCons a list -> f (AnyCons a acc) list) 
      AnyNil
-- reverseAnyList = go AnyNil
--   where
--     go acc AnyNil = acc
--     go acc (AnyCons a list) = go (AnyCons a acc) list

filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList f = \case
  AnyNil -> AnyNil
  AnyCons a list -> 
    if (f undefined) -- `f a` doesn't work: "Couldn't match expected type ‘a’ with actual type ‘a1’"
      then AnyCons a (filterAnyList f list)
      else filterAnyList f list


lengthAnyList :: AnyList -> Int
lengthAnyList = \case
  AnyNil -> 0
  AnyCons _ list -> 1 + lengthAnyList list

foldAnyList :: Monoid m => AnyList -> m
foldAnyList = \case
  AnyNil -> mempty
  AnyCons a list -> undefined

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList = \case
  AnyNil -> True
  _ -> False

instance Show AnyList where
  show = error "What about me?"





{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- | `input` is the existential type variable. We can only apply `input -> output` to it
-- to get `output`

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

instance Eq output => Eq (TransformableTo output) where
  TransformWith f x == TransformWith g y = f x == g y

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?

instance Functor TransformableTo where
  fmap :: (output -> b) -> TransformableTo output -> TransformableTo b
  fmap f (TransformWith g x) = TransformWith (f . g) x




{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

bothEq :: EqPair -> Bool
bothEq (EqPair x y) = x == y

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

data EqPair' a where
   EqPair' :: Eq a => a -> a -> EqPair' a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

-- | Don't think you could represent as an ADT, because there's still the
-- type class constraint.




{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox int _)) = int

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers EmptyBox = 0
countLayers (IntBox _ e) = 1 + countLayers e
countLayers (StringBox _ i) = 1 + countLayers i
countLayers (BoolBox _ s) = 1 + countLayers s

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

removeLayer :: MysteryBox a -> Maybe (MysteryBox b)
removeLayer EmptyBox = Nothing
-- removeLayer (_ _ x) = Just x -- Can't do this (this was what I had initial pass)
-- removeLayer (IntBox _ xs) = Just xs -- Also can't do this

-- I didn't think of this one, had to look at solutions
data Layer a b where
  Int' :: Layer Int ()
  String' :: Layer String Int
  Bool' :: Layer Bool String

-- Layer establishes a dependency between MysteryBox a and MysteryBox b
unpeel :: Layer a b -> MysteryBox a -> MysteryBox b
unpeel Int' (IntBox _ xs) = xs
unpeel String' (StringBox _ xs) = xs
unpeel Bool' (BoolBox _ xs) = xs



{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

head :: HList (String, (Int, (Bool, ()))) -> String
head (HCons s _) = s

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe = error "Can't implement, as far as I can tell"

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

appendHList :: HList a -> HList b -> HList (a, b)
appendHList = undefined
-- appendHList HNil xs         = xs
-- appendHList HNil xs         = HCons () xs ???
-- appendHList (HCons a as) xs = HCons a (appendHList as xs)




{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HEmpty :: HTree Empty
  HBranch :: HTree l -> c -> HTree r -> HTree (Branch l c r)
-- | ???

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

deleteLeftSubtree :: HTree (Branch l c r) -> HTree (Branch Empty c r)
deleteLeftSubtree (HBranch _ x r) = HBranch HEmpty x r 

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!

instance Eq Empty where 
  _ == _ = True
instance (Eq l, Eq c, Eq r) => Eq (Branch l c r) where
  _ == _ = True

instance Eq (HTree a) where
  HEmpty == HEmpty = True
  _ == HEmpty = False
  HEmpty == _ = False
  HBranch l x r == HBranch l' x' r' = undefined


{- EIGHT -}
-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  ANil :: AlternatingList x y
  ACons :: x -> AlternatingList y x -> AlternatingList x y

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts = \case 
  ANil -> []
  ACons a ANil -> [a]
  ACons a (ACons _ xs) -> a : getFirsts xs

getSeconds :: AlternatingList a b -> [b]
getSeconds = \case
  ANil -> []
  ACons _ ANil -> []
  ACons _ (ACons b xs) -> b : getSeconds xs

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues xs = (fold (getFirsts xs), fold (getSeconds xs))

foldValues' :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues' = go (mempty, mempty)
  where 
    go acc@(x, y) = 
      \case
        ANil -> acc
        ACons a ANil -> (a <> x, y)
        ACons a (ACons b xs) -> (a <> x, b <> y)





{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval = 
  \case
    IntValue n  -> n
    BoolValue b -> b
    If a x y    -> if eval a then eval x else eval y
    Equals x y  -> eval x == eval y
    Add x y     -> eval x + eval y

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

-- parse :: DirtyExpr -> Maybe (Expr Int)
-- parse = 
--   \case
--     DirtyIntValue n  -> Just (IntValue n)
--     DirtyBoolValue b -> Nothing
--     DirtyIf a x y    -> 
--       case (a, x, y) of
--         ( DirtyBoolValue b , DirtyIntValue n , DirtyIntValue m ) -> Just ( (if b then n else m)
--         _                -> Nothing
--     DirtyEquals x y  -> eval x == eval y
--     DirtyAdd x y     -> eval x + eval y

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?





{- TEN -}
-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  TNil :: TypeAlignedList x x
  TCons :: (x -> y) -> TypeAlignedList y z -> TypeAlignedList x z

-- | b. Which types are existential?

-- | y ? 

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs TNil TNil = TNil
composeTALs x TNil = x
composeTALs TNil y = y
composeTALs y (TCons f fs) = TCons f (composeTALs y fs)