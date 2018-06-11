{-# OPTIONS -fplugin=WhatItDo #-}
import WhatItDo (traceDo)
main =
  traceDo (do
    a <- return ( 42 :: Int )
    b <- return ( a * 2 )
    f <- return ( \b -> return ( b - 1 ) )
    c <- f ( a * b )
    print c)
