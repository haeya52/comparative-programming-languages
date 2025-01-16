{-
Your Name: Hailey Lee
Partner:   N/A

-}

module Homework8 where

import Control.Monad
import Control.Monad.State      -- State, runState, get, put, guard

data SnocList a = Lin | Snoc (SnocList a) a deriving (Show, Ord)
data Tree a = L | V a | Br (Tree a) a (Tree a) deriving (Show)

type Name = String
type FamilyTree = [(Name,Name)]


-- =============================================================================

-- given above.
-- data SnocList a = Lin | Snoc (SnocList a) a deriving (Show, Ord)

{-
Implement the Eq typeclass so that a snoclist of equatable things is equivalent to 
another if all the items at the same positions are equivalent (==), and there are 
the same number of items in each list.
-}
instance Eq a => Eq (SnocList a) where
  (==) Lin Lin = True
  (==) (Snoc xs x) (Snoc ys y) = (x == y) && ((==) xs ys)
  (==) _ _ = False

{-
Implement the Functor typeclass; to fmap a function across a snoclist will 
preserve the shape and apply the function at each spot in the snoclist to generate 
the new snoclist.
-}
instance Functor SnocList where
  fmap f someSnoclist = case someSnoclist of
                          Lin -> Lin
                          Snoc xs x -> Snoc (fmap f xs) (f x)
{-
Given a snoclist, since it might have items in it or not, find the last item and 
return Just that if present; return Nothing otherwise. 
-}
snocLast :: SnocList a -> Maybe a
snocLast Lin = Nothing
snocLast (Snoc _ x) = Just x

{-
Multiply all the ints in the snoclist together. An empty snoclist would result in 1.
-}
snocProduct :: (Num a) => SnocList a -> a
snocProduct Lin = 1
snocProduct (Snoc xs x) = x * (snocProduct xs)

{-
Given a snoclist of orderable things, return the largest when present. 
-}
snocMax :: (Ord a) => SnocList a -> Maybe a
snocMax Lin = Nothing
snocMax (Snoc xs x) = Just (snocMaxHelper (Snoc xs x))

{-
Helper function for snocMax
-}
snocMaxHelper :: (Ord a) => SnocList a -> a
snocMaxHelper Lin = undefined
snocMaxHelper (Snoc Lin x) = x
snocMaxHelper (Snoc (Snoc ys y) x) = case compare x y of
                                      LT -> snocMaxHelper (Snoc ys y)
                                      _ -> snocMaxHelper (Snoc ys x)

{-
Given two snoclists, return a snoclist of the longest-matching suffix.
-}
longestSnocSuffix :: (Eq a) => SnocList a -> SnocList a -> SnocList a
longestSnocSuffix Lin _ = Lin
longestSnocSuffix _ Lin = Lin
longestSnocSuffix (Snoc xs x) (Snoc ys y) = if x == y
                                            then Snoc (longestSnocSuffix xs ys) x
                                            else longestSnocSuffix xs ys
{-
Given two snoclists, zip them together from the end towards the front, with the 
answer no longer than the shortest list of the two.
-}
snocZip :: SnocList a -> SnocList b -> SnocList (a,b)
snocZip _ Lin = Lin
snocZip Lin _ = Lin
snocZip (Snoc xs x) (Snoc ys y) = Snoc (snocZip xs ys) (x, y)

{-
Given a regular list, convert it into a snoclist.
-}
snocify :: [a] -> SnocList a
snocify [] = Lin
snocify a = snocifyHelper (reverse a)

{-
Helper function for snocify
-}
snocifyHelper :: [a] -> SnocList a
snocifyHelper [] = Lin
snocifyHelper (x:xs) = Snoc (snocifyHelper xs) x

{-
Given a snoclist, convert it into a regular list, preserving the values 
and ordering.
-}
unSnocify :: SnocList a -> [a]
unSnocify Lin = []
unSnocify (Snoc xs x) = (unSnocify xs) ++ [x]

{-
iven a snoclist of ints, return the list of items that are unique in the original 
snoclist; preserve ordering of the last occurrence of each unique value. 
-}
uniques :: SnocList Int -> SnocList Int
uniques Lin = Lin
uniques a = snocify (reverse (uniquesHelper [] a))

{-
Helper function for uniques
-}
uniquesHelper :: [Int] -> SnocList Int -> [Int]
uniquesHelper l Lin = l
uniquesHelper l (Snoc xs x) | elem x l = uniquesHelper l xs
                            | otherwise = uniquesHelper (l ++ [x]) xs

{-
Reverse the contents of a snoclist
-}
snocReverse :: SnocList a -> SnocList a
snocReverse Lin = Lin
snocReverse a = snocify (reverse (unSnocify a))

-- =============================================================================
 
-- given above.
-- data Tree a = L | V a | Br (Tree a) a (Tree a) deriving (Show)

{-
The two trees being compared must be the same shape with the equal values at each 
spot in order to be considered equal.
-}
instance (Eq a) => Eq (Tree a) where
  (==) L L = True
  (==) (V a) (V b) = a == b
  (==) (Br xl x xr) (Br yl y yr) = (x == y) && ((==) xl yl) && ((==) xr yr)
  (==) _ _ = False

{-
Given two trees, return an Ordering value (LT, EQ, or GT) that relates them
-}
instance (Ord a) => Ord (Tree a) where
  compare L L = EQ
  compare L _ = LT
  compare _ L = GT
  compare (V a) (V b) | a == b = EQ
                      | a < b = LT
                      | otherwise = GT
  compare (V a) _ = LT
  compare _ (V b) = GT
  compare (Br xl x xr) (Br yl y yr) = compare xl yl

{-
Given a value and a tree that is ordered (exhibits an ordered in-order traversal), 
insert the value in the first in-order position available that preserves in-order 
ordering.
-}
insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree n L = V n
insertTree n (V x) = case compare n x of
                            GT -> Br L x (V n)
                            _ -> Br (V n) x L
insertTree n (Br xl x xr) = case compare n x of
                          GT -> Br xl x (insertTree n xr)
                          _ -> Br (insertTree n xl) x xr

{-
Walk the tree in-order, and generate the list of values as they are visited.
-}
inOrder :: Tree a -> [a]
inOrder L = []
inOrder (V x) = [x]
inOrder (Br xl x xr) = inOrder xl ++ [x] ++ inOrder xr

{-
Insert all the items of a list into a tree, then walk it inOrder to get the 
sorted list.
-}
treeSort :: (Ord a) => [a] -> [a]
treeSort [] = []
treeSort x = inOrder (treeSortHelper x)

{-
Helper function for treeSort
-}
treeSortHelper :: (Ord a) => [a] -> Tree a
treeSortHelper [] = L
treeSortHelper (x:xs) = insertTree x (treeSortHelper xs)

{-
Given a tree of values, give back the maybe minimum value. This tree is not 
guaranteed to have any internal ordering.
-}
treeMin :: (Ord a) => Tree a -> Maybe a
treeMin a = treeMinHelper (inOrder a) Nothing

{-
Helper function for treeMin
-}
treeMinHelper :: (Ord a) => [a] -> Maybe a -> Maybe a
treeMinHelper [] _ = Nothing
treeMinHelper t x = Just (minimum t)


-- =============================================================================
-- Maybe Monads

-- useful for some testing; not actualy a required definition, and the
-- tester doesn't import this (it makes its own copy).

family = [
  ("Animal", "Object"),
  ("Cat","Animal"),
  ("Dog","Animal"),
  ("Siamese","Cat"),
  ("Calico","Cat"),
  ("Labrador","Dog"),
  ("Pug","Dog"),
  ("Book","Object"),
  ("Garbage","Can")
  ]

-- given above.
-- type Name = String
-- type FamilyTree = [(Name,Name)]


-- Maybe Monad

{-
Given a name of a Java class, a FamilyTree to look for this class's single parent's 
name, dig through and either find it (returning Just the parent's name) or fail to 
find it (returning Nothing).
-}
parent :: Name -> FamilyTree -> Maybe Name
parent "Object" _ = Nothing
parent _ [] = Nothing
parent n (x:xs) | n == (fst x) = Just (snd x)
                | otherwise = parent n xs

{-
NOT WORKING
You must use do-notation and the Maybe monad for this recursive definition that calls 
upon parent to construct the list of ancestors from the given family tree. When we 
reach Object, we are done searching; if any step of ancestry can't find a parent, we 
are done (with Nothing to return).
-}
ancestors :: Name -> FamilyTree -> Maybe [Name]
ancestors n x = do
  p <- lookup n x
  case p of
    "Nothing" -> Nothing
    "Object" -> Just []
    _ ->  do
      a <- ancestors p x
      return (n:a)


{-do 
  a <- ancestorsHelper1 (ancestors n x)
  if ((a == []) or (head(reverse a) /= Just "Object"))
    then return Nothing
    else return Just (ancestorsHelper2 a)

{-
Helper funcitons for ancestors
1: get a maybe list of ancestors
2: check either the name is existed or not and put Maybe 
-}
ancestorsHelper1 :: Maybe Name -> FamilyTree -> [Maybe Name]
ancestorsHelper1 n x = case parent n x of
                          Nothing -> []
                          Just "Object" -> [Just "Object"]
                          Just p -> [Just p] ++ ancestorsHelper1 (parent p x)

ancestorsHelper2 :: [Maybe Name] -> [Name]
ancestorsHelper2 [Just "Object"] = ["Object"]
ancestorsHelper2 ((Just x):xs) = [x] ++ ancestorsHelper2 xs-}

{-
Given a list, return Just the first item if it exists, otherwise Nothing.
-}
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:xs) = Just x

{-
NOT WORKING
You must use do-notation and the Maybe monad for this definition. Given two names, 
what is the most specific type that they share in their chains of inheritance? 
Either one might not have successfully been mapped all the way to Object, yielding Nothing.
-}
leastUpperBound :: Name -> Name -> FamilyTree -> Maybe Name
leastUpperBound n1 n2 x = Nothing
  {-n1 <- ancestors n1 x
  n2 <- ancestors n2 x
  return undefined-}



-- =============================================================================
-- State Monads

{-
We accept one int argument n, and we will use the State monad to store the three previous 
tribonacci numbers as the state. We eventually return the nth tribonacci item when we're 
done recursing on smaller and smaller values for n.
-}
tribM :: Int -> State (Int,Int,Int) Int
tribM 0 = do
  (a, b, c) <- get
  return a
tribM n = do
  (a, b, c) <- get 
  put (b, c, a + b + c)
  temp <- tribM (n - 1)
  return temp

{-
Use runState or evalState to successfully run your tribM definition and return that 
nth tribonacci number.
-}
trib :: Int -> Int
trib n = case runState (tribM n) (1, 1, 1) of
          (i, j) -> i

--------------------------------------------------------------------------------
{-
NOT WORKING
non-monadic version. You learn the calculations involved in a supposedly more familiar 
environment. You may want to use helper functions here.
-}
simpleBalanced :: String -> Bool
simpleBalanced "" = True
simpleBalanced "()" = True
simpleBalanced "{}" = True
simpleBalanced "[]" = True
simpleBalanced _ = False

{-
monadic version that stores a stack of unclosed pairings that have been opened earlier 
in the string; the Bool result answers if the entire string is balanced or not.
-}
balancedM :: String -> State [Char] Bool
balancedM [] = do
  return undefined

{-
Use balancedM definition here to implement the same functionality as simpleBalanced.
-}
balanced :: String -> Bool
balanced s = False

-- =============================================================================
-- List Monads

{-
inds all the divisors of the first argument. Start by building the list of all candidate 
divisors (e.g. [a..b] syntax), and the list monad should be picking each one and guarding 
for which ones are actually divisors.
-}
divisors :: Int -> [Int]
divisors n = do
  d <- [x | x <- [1..n], mod n x == 0]
  return d

{-
Given a starting value and a factor, generates the infinite sequence of numbers produced 
by multiplying previous values by the factor.
-}
geometric :: Int -> Int -> [Int]
geometric n step = do
  g <- [n * step ^ x | x <- [0..]]
  return g

{-
The (infinite) list of Mersenne numbers M_1, M_2, M_3, … . They are not all prime, though 
the largest prime number known happens to be a Mersenne number. Each Mersenne number is 
of the form M_n = 2^n - 1.
-}
mersennes :: [Int]
mersennes = do
  m <- [2 ^ x - 1 | x <- [1..]]
  return m

{-
All sides of a triangle must be less than the sum of the other two legs (or else they couldn't 
reach each other). Given an upper limit n, create all the triplets of triangle side lengths 
that are whole numbers, from shortest to longest.
-}
unitTriangles :: Int -> [(Int,Int,Int)]
unitTriangles n = do
  u1 <- [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], z < x + y]
  u2 <- unitTrianglesHelper [u1]
  return u1

{-
Helper function for unitTriangles
-}
unitTrianglesHelper :: [(Int, Int, Int)] -> [(Int,Int,Int)]
unitTrianglesHelper [(a, b, c)] = [(a, b, c)]
unitTrianglesHelper ((a, b, c):xs) = if (elem (b, a, c) xs)
                                     then unitTrianglesHelper xs
                                     else [(a, b, c)] ++ unitTrianglesHelper xs

--------------------------------------------------------------------------------

