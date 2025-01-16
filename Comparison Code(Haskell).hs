-- Name: Hailey Lee
-- NetID: G01254390


-- please add comments to your code below as usual.


module Homework4 where

import Prelude hiding (zipWith,any)
--------------------------------------------------------------------------------
{-
 Given a positive integer, return a list of its prime factors, in increasing order. 
-} 

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors p = let
    --check if the number is a prime number
    isPrime :: Int -> [Int] -> Bool
    isPrime n (x:xs) | n == x           = True
                     | mod n x == 0     = False
                     | otherwise        = isPrime n xs

    --add prime numbers to the list
    addPrimes :: Int -> Int -> [Int]
    addPrimes x y | x == (y+1)                           = []
                  | (mod y x == 0) && isPrime x [2..x]   = x : addPrimes 2 (div y x)
                  | otherwise                            = addPrimes (x+1) y 
    
    in
      addPrimes 2 p

--------------------------------------------------------------------------------
{-
 Given two positie integers, return whether they are co-prime or not.
-}

coprime :: Int -> Int -> Bool
coprime 1 _ = False
coprime _ 1 = False
coprime x y = let
    factor :: Int -> Int -> [Int] -> Bool
    factor _ _ []                                               = True
    factor a b (x:xs) | (mod a x == 0) && (mod b x == 0)        = False
                      | otherwise                               = factor a b xs

    in
      factor x y [2..(min x y)]

--------------------------------------------------------------------------------
{-
 Given a non-negative integer n, calculate the nth tribonnaci number (where values
 are the sums of the previous three items in the sequence, or 1 when there aren't
 enough values ahead it.)
-}

trib :: Int -> Int
trib 0 = 1
trib 1 = 1
trib 2 = 1
trib t = let
    tribHelp :: Int -> Int -> Int -> Int -> Int
    tribHelp a b c d | (d - 3) <= 0 = (a + b + c)
                     | otherwise   = tribHelp b c (a + b + c) (d - 1)
    in
      tribHelp 1 1 1 t
--------------------------------------------------------------------------------
{-
 Given a list of integers xs, find the largest values and return them in a list
 (largest first). Largest duplicates should be preserved (included) in the output.
-}

maxTwo :: [Int] -> [Int]
maxTwo []     = []
maxTwo [x]    = [x]
maxTwo [x, y] = [x, y]
maxTwo (x:y:xs) = let
    maxTwoHelp :: Int -> Int -> [Int] -> [Int]
    maxTwoHelp f s []     | f == s          = [f, s]
                          | otherwise       = [(max f s), (min f s)]
    maxTwoHelp f s (x:xs) | x <= f && x <= s  = maxTwoHelp f s xs
                          | otherwise       = maxTwoHelp (max f s) x xs
    in
      maxTwoHelp x y xs
--------------------------------------------------------------------------------
{-
 Given a list of any type of values, create the list whose values are in the
 opposite order.-}

reversed :: [a] -> [a]
reversed []     = []
reversed (x:xs) = reversed xs ++ [x]

--------------------------------------------------------------------------------
{-
 Given a list of lists, assume it is rectangular (all rows have same length),
 create and return the list of lists that contains the same values, but rotated
 clockwise.
-}

clockwise :: [[Int]] -> [[Int]]
clockwise []   = []
clockwise [[m]] = [[m]]
clockwise m = let
    --number of rows
    numRow :: [[Int]] -> Int
    numRow []     = 0
    numRow (x:xs) = 1 + numRow xs
    
    i :: Int
    i = numRow m

    --numbr of columns
    numCol :: [Int] -> Int
    numCol []     = 0
    numCol (x:xs) = 1 + numCol xs

    j :: Int
    j = numCol (m!!0)

    --extract an element from each row
    extract :: Int -> Int -> [[Int]] -> [Int]
    extract 0 _ _          = []
    extract row col (x:xs) = extract (row - 1) col xs ++ [x!!col] 
    
    --loop through columns
    loop :: [Int] -> [[Int]]
    loop []     = []
    loop (c:cs) = [extract i c m] ++ loop cs 

    in
      loop [0..(j-1)]
 
--------------------------------------------------------------------------------
{-
 Given a list of Bool values, return True if any of them is True, and return
 False if every single one of them is False.
-}

any :: [Bool] -> Bool
any []                  = False
any (x:xs) | x == True  = True
           | otherwise  = any xs

--------------------------------------------------------------------------------
{-
 Given a predicate function (::a->Bool) as well as a list of values (::[a]),
 create a list of all items from the argument list that pas the predicate
 function. Preserve ordering in your output.
-}

select :: (a->Bool)-> [a] -> [a]
select f []     = []
select f (x:xs) = case (f x) of
                     True  -> x : select f xs
                     False -> select f xs
                              
--------------------------------------------------------------------------------
{-
 Given a two-argument function and two lists or arguments to supply, creat a list
 of the results of applying the function to each same-indexed pair of arguments
 from the two lists. (Zip up the two lists with the function)
-}

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f _ []          = []
zipWith f [] _          = []
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs

--------------------------------------------------------------------------------
{-
 Given positive ints r and c indicating number of rows and columns, create a 2D
 list that represents the "augmented identity matrix" with that dimention: It's
 the k x k identity matrix (where k = min(r, c)), and augmented rightwards or
 downwards as needed with zeroes in order to be of size r x c. 
-}

augdentity :: Int -> Int -> [[Int]]
augdentity 0 _ = [[]]
augdentity _ 0 = [[]]
augdentity r c = let
    --fill out each row
    fill :: Int -> [Int] -> [Int]
    fill _ []                 = []
    fill x (y:ys) | x == y    = 1 : fill x ys
                  | otherwise = 0 : fill x ys

    --combine rows
    combine :: [Int] -> [[Int]]
    combine []     = []
    combine (a:as) = [fill a [0..(c-1)]] ++ combine as

    in
      combine [0..(r-1)]

--------------------------------------------------------------------------------
