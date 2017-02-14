# Understanding Folds

1. C

3. C

4. B

5. a. `foldr (++) "" ["woot", "WOOT", "woot"]`

   b. `foldr max 'a' "fear is the little death"

   c. `foldr (&&) True [False, True]`

   d. `foldr (||) True [False, True]`

   e. `foldl (\acc x -> acc ++ show x) "" [1..5]`

   f. `foldr (\x acc -> const acc x) 'a' [1..5]`

   g. `foldr (\x acc -> const acc x) 0 "tacos"`

   h. `foldl (\acc x -> flip const x acc) 0 "burritos"`

   i. `foldl (\acc x -> flip const x acc) 'z' [1..5`



