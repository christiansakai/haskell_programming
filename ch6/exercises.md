# Eq Instances

In source code eqInstances.hs

# Tuple Experiment

```
quotRem :: Integral a => a -> a -> (a, a)
divMod :: Integral a => a -> a -> (a, a)
```

`quotRem` function returns the quotient and remainder in a two-tuple, while `divMod` returns the division and modulus.

# Will They Work?

1. Will work

2. Will work

3. Will not work, because `True` compared to `"Julie"` is not implemented

4. Will work 

# Chapter Exercises
## Multiple choice

1. C

2. A

3. A

4. C

5. A

## Does it typecheck?

Source code doesItTypeCheck.hs

## Given a datatype declaration, what can we do?

Source code givenADataType.hs

## Match the types

1. Can't substitute, because `Int` is more concrete than `Num a`

2. Can't because `Float` is more concrete than `Num a`

3. It can.

4. It can.

5. It can.

6. It can

7. It can't. Because a is too general and `Int` is more concrete.

8. It can't. Because `Num a` is too general and `Int` is more concrete.

9. It can.

10. It can.

11. It can't.

# Type Kwon Do 2

1. ```
   chk :: Eq b => (a -> b) -> a -> b -> Bool
   chk f a b = Bool
   ```

2. ```
   arith :: Num b => (a -> b) -> Integer -> a -> b
   arith f x a = f a
   ```
