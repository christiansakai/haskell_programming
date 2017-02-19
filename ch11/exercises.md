# Dog Types

1. Type constructor

2. `* -> *`

3. `*`

4. `Doggies Int`

5. `Doggies Integer`

6. `Doggies String`

7. Both

8. `* -> *`

9. `DogueDebordeaux String`

# Cardinality

1. 1

2. 3

3. 2 times of Int8

4. There is minBound and maxBound for Int but not for Integer

5. It is because 8 represents 8 bits (1 byte), which can contain up to 256 integers (-128 to 127)

# For Example

1. a. Example

   b. Error happened instead

2. Yeah, the typeclass instance is Example

3. It becomes a function `MakeExample :: Int -> Example`

# Pity the Bool

1. 2 + 2 = 4

2. a. 256 + 2 = 258

   b. It will give me warning saying that Literal 128 is out of the Int8 range

   c. It will give me warning saying that Literal -129 is out of the Int8 range

# How Does Your Garden Grow

1. ```
   data Garden = Gardenia Gardener
               | Daisy Gardener
               | Rose Gardener
               | Lilac Gardener
               deriving Show
   ```

# The Quad

1. `2 * 4 = 8`

2. `4 * 4 = 16`

3. `4 ^ 4 = 256`

4. `2 * 2 * 2 = 8`

5. `(2 ^ 2) ^ 2 = 16`

6. `(4 ^ 4) ^ 2 = 65536`

# Chapter Exercises
## Multiple choice

1. A

2. C

3. B

4. C

## Ciphers

Source code vigenereCipher.hs
