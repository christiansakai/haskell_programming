# Type Matching

* `not :: Bool -> Bool`

* `length :: [a] -> Int`

* `concat :: [a] -> [a]`

* `head :: [a] -> a`

* `(<)` :: (Ord a) => a -> a -> Bool`

# Type Arguments

1. `f :: Char -> Char -> Char`

2. `Char`

3. `Num b => b`

4. `Double`

5. `[Char]`

6. `Eq b => b -> [Char]`

7. `(Num a, Ord a) => a`

8. `(Num a, Ord a) => a`

9. `Integer`

# Parametricity

1. Impossible

2. It is 

   `(a -> a) -> a` or 

   `a -> (a -> a)`

   We cannot violate the constraints

3. It is

   `a -> (b -> b)` is the only possiblle implementation

# Apply Yourself

1. ```
   (++) :: [a] -> [a] -> [a]

   myConcat x = x ++ " yo"
   ```

   It will become

   ```
   (++) :: [Char] -> [Char] -> [Char]
   ```

2. ```
   (*) :: Num a => a -> a -> a

   myMult x = (x / 3) * 5
   ```

   It will become 

   ```
   (*) :: Fractional a => a -> a -> a
   ```

3. ```
   take :: Int -> [a] -> [a] 

   myTake x = take x "hey you"
   ```

   It will become

   ```
   take :: Int -> [Char] -> [Char]
   ```

4. ```
   (>) :: Ord a => a -> a -> Bool

   myCom x = x > (length [1..10])
   ```

   It will become

   ```
   (>) :: Int -> Int -> Bool
   ```

5. ```
   (<) :: Ord a => a -> a -> Bool

   myAlph x = x < 'z'`
   ```

   It will become

   ```
   (<) :: Char -> Char -> Bool
   ```

# Chapter Exercises
## Multiple Choice

1. C

2. A

3. B

4. C 

## Determine the Types

1. Determine the value and type of that value

  a. ```
     54 :: Num a => a
     ```

  b. ```
     (0, "doge") :: Num t => (t, [Char])
     ```

  c. ```
     (0, "doge") :: (Int, [Char])
     ```

  d. ```
     True :: Bool
     ```

  e. ```
     5 :: Int
     ```

  f. ```
     False :: Bool
     ```

2. `150 :: Num a => a`

3. `z 10 :: Num a => a -> a`

4. `0.4 :: Fractional a => a`

5. `Julie <3 Haskell :: [Char]`

## Does it compile?

1. ```
   bigNum = (^) 5
   wahoo = bigNum $ 10
   ```

2. This function is already correct

3. ```
   a = (+)
   b = 5
   c = a 10
   d = c 200
   ```

4. Cannot be fixed without `c`

## Type variable or specific type constructor?

2. `Fully -> Concrete -> Concrete`

3. `Fully -> Constrained -> Concrete`

4. `Fully -> Fully -> Concrete`

## Write a type signature

1. ```
   functionH :: [a] -> a
   functionH (x:_) = x
   ```

2. ```
   functionC :: Eq a => a -> a -> Bool
   functionC x y = if (x > y) then True else False
   ```

3. ```
   functionS :: (a, b) -> b
   function (x, y) = y
   ```

## Given the type, write the function

1. ```
   i :: a -> a
   i x = x
   ```

2. ```
   c :: a -> b -> a 
   c x y = x
   ```

3. The same thing

   ```
   c'' :: b -> a -> b
   c'' = c
   ```

4. ```
   c' :: a -> b -> b
   c' x y = y
   ```

5. ```
   r :: [a] -> [a]
   r xs = xs

   r :: [a] -> [a]
   r xs = tail xs
   ```

6. ```
   co :: (b -> c) -> (a -> b) -> a -> c
   co bToc aTob a = bToc . aTob a
   ```

7. ```
   a :: (a -> c) -> a -> a
   a f x = x
   ```

8. ```
   a' :: (a -> b) -> a -> b
   a' f a = f a
   ```

## Fix it

Code is in actual source code

## Type-Kwon Do

1. ```
   f :: Int -> String
   f = undefined

   g :: String -> Char
   g = undefined

   h :: Int -> Char
   h = g . f
   ```

2. ```
   data A
   data B
   data C

   q :: A -> B
   q = undefined

   w :: B -> C
   w = undefined

   e :: A -> C
   e = w . q
   ```

3. ```
   data X
   data Y
   data Z

   xz :: X -> Z
   xz = undefined

   yz :: Y -> Z
   yz = undefined

   xform :: (X, Y) -> (Z, Z)
   xform (x, y) = (xz x, yz y) 
   ```

4. ```
   munge :: (x -> y) -> (y -> (w, z)) -> x -> w
   munge xToy yTowz = fst . yTowz . xToy
   ```

