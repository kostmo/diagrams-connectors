{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

-- | Generates a <https://en.wikipedia.org/wiki/Rectilinear_Steiner_tree#Single-trunk_Steiner_trees minimum single-trunk Steiner tree (MSTST)>.
module Diagrams.TwoD.OrthoConnect.StraightBusConnectors (
    renderStraightBusConnector
  , VerticalBusPosition (..)
  ) where

import           Data.List                         (sort)
import qualified Data.List.NonEmpty                as NE
import qualified Data.Maybe                        as Maybe
import           Diagrams.Prelude                  as DP

import qualified Diagrams.TwoD.OrthoConnect.Coords as Coords
import qualified Diagrams.TwoD.OrthoConnect.Style  as Style


data VerticalBusPosition = Median | Average
  deriving (Bounded, Enum, Eq, Show)


-- | Compare to 'Diagrams.TwoD.OrthoConnect.RightAngleConnectors.renderFixedConnector'.
--
-- Generates only horizontal buses.
renderStraightBusConnector :: (IsName a, Monoid m, Semigroup m, TrailLike (QDiagram b V2 Double m)) =>
     VerticalBusPosition -- ^ vertical positioning method
  -> [a]
  -> QDiagram b V2 Double m
  -> QDiagram b V2 Double m
renderStraightBusConnector vertical_positioning_method subdiagram_names d =

  d <> additions <> bus_segment
  where

    additions = mconcat $ NE.toList stem_segments

    subdiagrams = NE.fromList $ Maybe.mapMaybe (`lookupName` d) subdiagram_names

    all_center_points = NE.map centerPoint subdiagrams

    all_y_coords = NE.map Coords.getY all_center_points

    y_function = case vertical_positioning_method of
      Median  -> median
      Average -> average

    y_position = y_function all_y_coords


    all_x_coords = NE.map Coords.getX all_center_points
    leftmost_x = minimum all_x_coords
    rightmost_x = maximum all_x_coords

    stem_segments = NE.map (renderStem y_position) all_center_points

    left_bus_point = p2 (leftmost_x, y_position)
    right_bus_point = p2 (rightmost_x, y_position)
    bus_segment = renderSegment left_bus_point right_bus_point


renderStem :: (Monoid m, Semigroup m, TrailLike (QDiagram b V2 Double m)) =>
     Double
  -> Coords.Point2D
  -> QDiagram b V2 Double m
renderStem y_position origin_point =
  renderSegment origin_point destination_point
  where
    destination_point = p2 (Coords.getX origin_point, y_position)


renderSegment :: (Monoid m, Semigroup m, TrailLike (QDiagram b V2 Double m)) =>
     Coords.Point2D
  -> Coords.Point2D
  -> QDiagram b V2 Double m
renderSegment origin_point destination_point =
  connector_trail # lwL Style.baseLineWeight
  where
    connector_trail = fromVertices [
        origin_point
      , destination_point
      ]


median :: (Ord a, Fractional a) => NE.NonEmpty a -> a
median xs = if even list_length
  then (odd_median + (sorted_list !! (half_length - 1))) / 2
  else odd_median
  where
    list_length = length xs
    half_length = list_length `div` 2
    sorted_list = sort $ NE.toList xs
    odd_median = sorted_list !! half_length


average :: Fractional a => NE.NonEmpty a -> a
average xs = sum xs / fromIntegral (length xs)
