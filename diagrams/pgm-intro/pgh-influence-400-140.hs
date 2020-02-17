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
main = mainWith $ pgh_1 ||| strutX 10 ||| pgh_2

--------------------- Diagrams  -------------------------

pgh_1 :: Diagram B
pgh_1 =
  vsep 8
    [ cn "P"
    , centerX $ hsep 8 [cn "G", cn "H"]
    ]
    # connectOutside "P" "G"
    # connectOutside "P" "H"
    # connectPerim' thickRedDashedArrow
      "G" "P" (7/32 @@ turn) (5/8 @@ turn)
    # connectPerim' thickRedDashedArrow
      "P" "H" (7/8 @@ turn) (9/32 @@ turn)

pgh_2 :: Diagram B
pgh_2 =
  vsep 8
    [ cn' "P"
    , centerX $ hsep 8 [cn "G", cn "H"]
    ]
    # connectOutside "P" "G"
    # connectOutside "P" "H"
    # connectPerim' failedArrow
      "G" "P" (7/32 @@ turn) (5/8 @@ turn)
    # connectPerim' failedArrow
      "P" "H" (7/8 @@ turn) (9/32 @@ turn)

--------------------- Diagram utilities  -------------------

failedArrow =
  arr' & colorArr' red' 0.7 & shaftStyle %~ dashingL [0.2, 0.4] 0 . lw thin

thickRedDashedArrow =
  arr' & colorArr red' & shaftStyle %~ dashingL [0.4, 0.2] 0 . lw thick

red' = darken 0.7 red

cn :: String -> QDiagram B V2 Double Any
cn k =
  txt k <> c
  # named k

cn' :: String -> QDiagram B V2 Double Any
cn' k =
  txt k <> c'
  # named k

c :: QDiagram B V2 Double Any
c = circle 4 # fc silver

c' :: QDiagram B V2 Double Any
c' = circle 3.7 # fc lightblue <> c

txt s = text s # font "serif" . fontSizeL 3

arr = arr' & arrowShaft .~ arc xDir (-1/6 @@ turn)

arr' =
  with & arrowHead .~ spike
       & headLength .~ normal

colorArr col =
  (headStyle %~ fc col)
  . (tailStyle %~ fc col)
  . (shaftStyle %~ lc col)

colorArr' col o =
  (headStyle %~ fc col . opacity o)
  . (tailStyle %~ fc col . opacity o)
  . (shaftStyle %~ lc col . opacity o)
