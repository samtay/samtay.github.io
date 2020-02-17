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
import Diagrams.TwoD.Arrow
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine

--------------------- Write files  -------------------------

main :: IO ()
main = mainWith $ trails ||| strutX 10 ||| condTrails

--------------------- Diagrams  -------------------------

trails :: Diagram B
trails =
  vsep 12
    [ trail # connOutside "X" "Y" # connOutside "Y" "Z"
        # influenced "X" "Y" # influenced "Y" "Z"
    , trail # connOutside "Y" "X" # connOutside "Y" "Z"
        # influenced "Y" "X" # influenced "Y" "Z"
    , trail # connOutside "X" "Y" # connOutside "Z" "Y"
        # notInfluenced "X" "Y" # notInfluenced "Z" "Y"
    ]

condTrails :: Diagram B
condTrails =
  vsep 12
    [ trail' # connOutside "X" "Y" # connOutside "Y" "Z"
        # notInfluenced "X" "Y" # notInfluenced "Y" "Z"
    , trail' # connOutside "Y" "X" # connOutside "Y" "Z"
        # notInfluenced "Y" "X" # notInfluenced "Y" "Z"
    , trail' # connOutside "X" "Y" # connOutside "Z" "Y"
        # influenced "X" "Y" # influenced "Z" "Y"
    ]

connOutside = connectOutside' arr

influenced = mkInfluence thickRedDashedArrow

notInfluenced = mkInfluence failedArrow

mkInfluence arrType n1 n2 =
  let (startAngle, endAngle) =
        if n1 < n2 then (1/20, 9/20) else (9/20, 1/20)
  in connectPerim' arrType n1 n2 (startAngle @@ turn) (endAngle @@ turn)

trail :: Diagram B
trail = hsep 12 [cn "X", cn "Y", cn "Z"]

trail' :: Diagram B
trail' = hsep 12 [cn "X", cn' "Y", cn "Z"]

--------------------- Diagram utilities  -------------------

failedArrow =
  arr' & colorArr' 0.7 red' & headStyle %~ opacity 0.5 & shaftStyle %~ dashingL [0.3, 0.4] 0 . lw thin

thickRedDashedArrow =
  arr' & colorArr red' & shaftStyle %~ dashingL [0.8, 0.4] 0 . lw thick

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
c = circle 3.5 # fc silver

c' :: QDiagram B V2 Double Any
c' = circle 3.1 # fc lightblue <> c

txt s = text s # font "serif" . fontSizeL 2.5

arr = with & headLength .~ small

arr' =
  with & arrowHead .~ spike
       & headLength .~ normal

colorArr = colorArr' 1

colorArr' o col =
  (headStyle %~ fc col . opacity o)
  . (tailStyle %~ fc col . opacity o)
  . (shaftStyle %~ lc col . opacity o)
  . (headLength .~ small)
