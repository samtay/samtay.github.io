#!/usr/bin/env stack
{- stack runghc
  --install-ghc
  --resolver lts-14.21
  --package diagrams
-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}
module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine

--------------------- Write files  -------------------------

main :: IO ()
main = mainWith pgh_1

--------------------- Diagrams  -------------------------

pgh_1 :: Diagram B
pgh_1 =
  vsep 8
    [ cn "P"
    , centerX $ hsep 8 [cn "G", cn "H"]
    ]
    # connectOutside "P" "G"
    # connectOutside "P" "H"

--------------------- Diagram utilities  -------------------

cn :: String -> QDiagram B V2 Double Any
cn k =
  txt k <> c
  # named k

c :: QDiagram B V2 Double Any
c = circle 4 # fc silver

c' :: QDiagram B V2 Double Any
c' = circle 3.7 # fc lightblue <> c

txt s = text s # font "serif" . fontSizeL 3
