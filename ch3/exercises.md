# Scope

1. No

2. Yes

3. No

4. Yes

# Syntax Errors

1. `++ [1, 2, 3] [4, 5, 6]` => Will compile

2. `'<3' ++ ' Haskell'` => Snytax error

  `"<3" ++ " Haskell"`

3. `concat ["<3", " Haskell"]` => Will compile

# Chapter Exercises
## Reading Syntax

1. Decide if these code are correct:

  * `concat [[1, 2, 3], [4, 5, 6]]` => Correct
  * `++ [1, 2, 3] [4, 5, 6]` => Incorrect
  * `(++) "hello" " world"` => Correct
  * `["hello" ++ " world"]` => Correct
  * `4 !! "hello"` => Incorrect
  * `(!!) "hello" 4` => Correct
  * `take "4 lovely"` => Incorrect
  * `take 3 "awesome"` => Correct

2. What are the results of these evaluations

  * `concat [[1 * 6], [2 * 6], [3 * 6]]` => `[6, 12, 18]`
  * `"rain" ++ drop 2 "elbow"` => `"rainbow"`
  * `10 * head [1, 2, 3]` => `10`
  * `(take 3 "Julie") ++ (tail "yes")` => `"Jules"`
  * ```
    concat [tail [1, 2, 3],
            tail [4, 5, 6],
            tail [7, 8, 9]]
    ```
    will result in `[2, 3, 5, 6, 8, 9]`

## Building functions

1. Functions:

  * `"Curry is awesome" ++ "!"`
  * `"Curry is awesome" !! 4` 
  * `drop 9 "Curry is awesome!"`

The rest are in `chapterExercises.hs`
