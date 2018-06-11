{-# OPTIONS -fplugin=WhatItDo #-}

module Main where

import WhatItDo ( traceDo )

main :: IO ()
main = traceDo ( do
  a <- return (return () :: IO ())
  b <- getLine
  traceDo a
  return () )
