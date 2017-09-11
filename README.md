# Haskell Programming

Code and answers for the book [Haskell Programming from First Principles](http://haskellbook.com/)

Todo:
* Ch 15
  * Semigroup exercise
    * No. 10
  * Monoid exercise
    * No. 6, 7, 8

* Ch 17
  * Lookup
    * No. 4
  * Constant

* Ch 18
  * writeUsingFunctorMonad
    * No. 5, 6

* Starting from Ch 19 onward:
  * everything

__Template for scripting with Stack__

```
#!/usr/local/bin/stack
{- stack 
   --resolver lts-9.3 
   --install-ghc 
   exec ghci
   --package parsec
-}
```

