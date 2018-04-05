{-# LANGUAGE ViewPatterns #-}

-- | Coordinate member extraction
module Diagrams.TwoD.OrthoConnect.Coords where

import           Diagrams.Prelude


type Point2D = Point V2 Double


getX :: P2 t -> t
getX (coords -> x1 :& _) = x1


getY :: P2 t -> t
getY (coords -> _ :& y1) = y1
