# Mood Swing

`data Mood = Blah | Woot deriving Show`

1. Type constructor: `Mood`

2. We can use `Blah` or `Woot`

3. `Woot` is not a type constructor. Type signature needs to be a type constructor.

4. Fix the function

  ```hs
  changeMood Blah = Woot
  changeMood Woot = Blah
  ```

5. Source file `mood.hs`

# Find the Mistakes

1. No mistakes

2. `not (x == 6)`

3. No mistakes

4. `["Merry"] > ["Happy"]`

5. `"1, 2, 3" ++ "look at me!"`

# Chapter Exercises

```hs
awesome = ["Papuchon", "curry", ":)"]
alsoAWesome = ["Quare", "The Simons"]
allAwesome = [awesome, alsoAwesome]
```

Length is a function that takes a list and returns a result that tells how many items are in the list.

1. `length :: [a] -> Int`

2. Results of the following expression

  * `length [1, 2, 3, 4, 5]` => 5

  * `length [(1, 2), (2, 3), (3, 4)]` => 3

  * `length allAwesome` => 2

  * `length (concat allAwesome)` => 5

3. The `6 / 3` will succeed because both `6` and `3` is a type `Num`, which is a superclass of `Fractional`. Meanwhile, `length [1, 2, 3]` will give us type of `Int`, which is not a superclass of `Fractional`

4. We can use `div`

5. Type is `Bool` and result is `True`

6. Type is `Bool` and result is `False`

7. Bits of code:

  * `length allAWesome == 2` => `True`

  * `length [1, 'a', 3, 'b']` will not work because List needs to be the same type

  * `length allAwesome + length awesome` => `5`

  * `(8 == 8) && ('b' < 'a')` => `False`

  * `(8 == 8) && 9` will not work because `9` is not an instance of type class `Bool`

8. Palindrome checker

  ```hs
  isPalindrome :: (Eq a) => [a] -> Bool
  isPalindrome x = x == y
    where y = reverse x
  ```

9. Absolute value returner

  ```hs
  myAbs :: Integer -> Integer
  myAbs x = if (x < 0) then (-x) else x
  ```

10. Fill in the definition below

  ```hs
  f :: (a, b) -> (c, d) -> ((b, d), (a, c))
  f tupleOne tupleTwo = ((snd tupleOne, snd tupleTwo), (fst tupleOne, fst tupleTwo))
  ```

## Correcting Syntax

1. Function that adds `l` to the length of a string argument and returns that result

  ```hs
  x = (+)

  f xs = w `x` 1
    where w = length xs
  ```

2. Identity function

  ```hs
  \x = x
  ```

3. Get the value `1` from `[1, 2, 3]`

  ```hs
  \(x:xs) -> x
  ```

4. Return `1` from value `(1, 2)` 

  ```hs
  f (a, b) = a
  ```

## Match the function names to their types

1. Which the following is the type of `show`

  * `Show a => a -> String`

2. Which of the following is the type of `(==)`

  * `Eq a => a -> a -> Bool`

3. Which of the following is the type of `fst`

  * `(a, b) -> a`

4. Which of the following is the type of `(+)`

  * `(+) :: Num a => a -> a -> a`

