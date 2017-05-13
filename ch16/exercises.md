# Be Kind

1. What's the kind of `a`?

   `a -> a`

   `*`

2. What are the kinds of `b` and `T`? (The `T` is capitalized on purpose!)

   `a -> b a -> T (b a)`

   `b` has kind `* -> *`

   `T` has kind `* -> *`

3. What's the kind of `c`?

   `c a b -> c b a`

   `* -> * -> *`

# Chapter Exercise
## Decide the kind

1. Can't, because kind *

2. Can, because kind is * -> *

3. Can, because kind is * -> *

4. Can, because kind is * -> *

5. Can't, because kind is *
