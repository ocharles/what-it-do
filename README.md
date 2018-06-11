# Hey GHC, `what-it-do`?

`what-it-do` is a GHC source plugin that rewrites `do` expressions to `trace`
all binds. For example, given the following code:

```haskell
main =
  do 
    a <- return ( 42 :: Int )
    b <- return ( a * 2 )
    f <- return ( \b -> return ( b - 1 ) )
    c <- f ( a * b )
    print c
```

We can trace all binds by using `debugDo` from `what-it-do`, and enabling the plugin:

```haskell
{-# OPTIONS -fplugin=WhatItDo #-}
import WhatItDo (traceDo)
main =
  traceDo (do 
    a <- return 42
    b <- return ( a * 2 )
    f <- return ( \b -> return ( b - 1 ) )
    c <- f ( a * b )
    print c)
```

Now, when this code runs, we see:

```
(Test.hs:5:5-29) a = 42
(Test.hs:6:5-25) b = 84
(Test.hs:8:5-20) c = 3527
3527
```

Magic!

## Wha... how is this possible?

Under the hood, `what-it-do` has rewritten the original `do` expression to:

```haskell
main =
  do 
    a <- return ( 42 :: Int ) >>= \x -> trace ( show x ) ( return x )
    b <- return ( a * 2 ) >>= \x -> trace ( show x ) ( return x )
    f <- return ( \b -> return ( b - 1 ) )
    c <- f ( a * b ) >>= \x -> trace ( show x ) ( return x )
    print c
```

Notice that because the plugin has access to the type-checker, we only trace
things that can be shown. The binding to `f` doesn't get traced, because we
can't show what `f` is.
