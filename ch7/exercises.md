# Grab Bag

1. Everything

2. A

3. a. ```
      addOneIfOdd n = case odd n of
        True -> f n
        False -> n
        where f = \n -> n + 1
      ```

    b. ```
       addFive = \x -> \x -> (if x > y then y else x) + 5
       ```

    c. ```
       mflip f x y = f y x
       ```

# Variety Pack

1. a. `k :: (a0, a1) -> a0`

   b. `k2 :: [Char]`, not the same with `k1` or `k3`

   c. `k3`

2. ```
   f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
   f (a, b, c) (d, e, f) = ((a, d), (c, f))
   ```

# Case Practice

Source code casePractice.hs

# Artful Dodgy

Source code artfulDodgy.hs

2. 11

3. 22

4. 21

5. 12

6. 11

7. 21

8. 21

9. 22

10. 31

11. 23

# Guard Duty

1. It will be constantly producing `'F'` as the result

2. It will return `'C'`

3. B

4. List which elements implement Eq instance

5. `pal :: Eq a => [a] -> Bool`

6. C

7. Data that implements Ord

8. `numbers :: Ord a => a -> Int`

# Chapter Exercises
## Multiple choice

1. D

2. B

3. D

4. B

5. A

## Let's write code

Source code in letsWriteCode.hs
