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
    connectOutside' arr "1" "2"
  . connectOutside' (arr & arrowShaft .~ arc xDir (1.3/5 @@ turn)) "1" "3"
  . connectOutside' (arr & arrowShaft .~ arc xDir (1.4/5 @@ turn)) "1" "n"
  . connectOutside' arr "2" "3"
  . connectOutside' (arr & arrowShaft .~ arc xDir (-1.4/5 @@ turn)) "2" "n"
  . connectOutside' arr "3" "n"
  $ hsep 8 [cn (show k) | k <- [1..3]]
      ||| strutX 9 ||| ellipses
      ||| strutX 9 ||| cn "n"

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

arr =
  with & arrowHead .~ spike
       & headLength .~ normal
       & arrowShaft .~ arc xDir (-1/6 @@ turn)

ellipses = txt "..."
