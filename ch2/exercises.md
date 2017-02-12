# Comprehension Check

1. Given the following lines of code as they might appear in a source file, how would you change them to use them directly in the REPL?

  ```hs
  half x = x / 2
  ```

  ```sh
  Prelude> let half x = x / 2
  ```

  ```hs
  square x = x * x
  ```

  ```sh
  Prelude> let square x = x * x
  ```

2. Write one function that can accept one argument and work for all the following expression. Be sure to name the function.

  ```hs
  circleArea :: Int -> Fractional
  circleArea r = 3.14 * (r * r)
  ```

3. There is a value in Prelude called `pi`. Rewrite your function to use `pi` instead of `3.14`.

  ```hs
  circleArea :: Int -> Fractional
  circleArea r = pi * (r * r)
  ```

# Parenthesis and Association

Below are some pairs of functions that are alike except for parenthesization. Read them carefully and decide if the parentheses change the results of the function. Check your work in GHCi.

1. a) `8 + 7 * 9` = __71__

   b) `(8 + 7) * 9` = __135__

2. a) `perimeter x y = (x * 2) + (y * 2)`

   b) `perimeter x y = x * 2 + y * 2`

   The two functions above are equivalent

3. a) `f x = x / 2 + 9`

   This is the same as 

   `f x = (x / 2) + 9`

   b) `f x = x / (2 + 9)`

# A Head Code

1. `let x = 5 in x`

  `5`

2. `let x = 5 in x * x`

  `25`

3. `let x = 5; y = 6 in x * y`

  `30`

4. `let x = 3; y = 1000 in x + 3`

  `1003`

Rewrite with `where` clauses

1. `let x = 5 in x`

  `x where x = 5`

2. `let x = 5 in x * x`

  `x * x where x = 5`

3. `let x = 5; y = 6 in x * y`

  `x * y where x = 5; y = 6`

4. `let x = 3; y = 1000 in x + 3`

  `x + 3 where x = 3; y = 1000`

# Chapter Exercises
## Parenthesization

1. `2 + 2 * 3 - 1`
  
  `2 + (2 * 3) - 1`

2. `(^) 10 $ 1 + 1`

  `(^) 10 (1 + 1)`

3. `2 ^ 2 * 4 ^ 5 + 1`

  `((2^2) * (4^5)) + 1`

## Equivalent Expression
These following pairs of expression will return the same result when evaluated:

1. `1 + 1` and `2`

2. `10 ^ 2` and `10 + 9 * 10`

#3 More Fun with Functions

```hs
z = 7
x = y ^ 2
y = z + 8
waxOn = x * 5
```

So the result will make

`z = 7`

`y = 7 + 8 = 15`

`x = 15 ^ 2 = 225`

`waxOn = 1125` 


1. What will these evaluate to:

* `10 + waxOn` => `1135`

* `(+10) waxOn` => `1135`

* `(-) 15 waxOn` => `-1115`

* `(-) waxOn 15` => `1115`

2. What will this evaluate to:

* `let triple x = x * 3` => `675`

3. What will this evaluate to:

* `triple waxOn` => `3375`

4. Rewrite `waxOn` using `where` in your source file

  ```hs
  waxOn = x * 5
    where x = y ^ 2
          y = z + 8
          z = 7
  ```

5. Now to the same source file where you have `waxOn`, add the triple function.

  ```hs
  waxOn = x * 5
    where x = y ^ 2
          y = z + 8
          z = 7

  triple x = x * 3
  ```

6. Add `waxOff x = triple x` in the source file

  ```hs
  waxOn = x * 5
    where x = y ^ 2
          y = z + 8
          z = 7

  triple x = x * 3

  waxOff x = triple x
  ```

7. Loaded the file above, What is the result of:

* `waxOff 10` => `30`

* `waxOff (-50)` => `-150`
