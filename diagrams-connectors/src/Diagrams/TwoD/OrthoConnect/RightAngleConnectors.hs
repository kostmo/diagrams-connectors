{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

-- | = Rules for consolidating arrows
--
-- The "last" element in the ordering that contains a node gets to own the arrow
-- for that node.
module Diagrams.TwoD.OrthoConnect.RightAngleConnectors (renderFixedConnector) where

import           Data.Colour.Palette.ColorSet
import           Diagrams.Prelude                       as DP

import qualified Diagrams.TwoD.OrthoConnect.BasicUtils  as BasicUtils
import qualified Diagrams.TwoD.OrthoConnect.Positioning as Positioning
import qualified Diagrams.TwoD.OrthoConnect.Style       as Style


-- | Draws with the builtin 'triangle' function, then positions and orients
-- it along the segment defined by the two points.
generateTriangleEndpoint :: (Monoid m, Semigroup m, TrailLike (QDiagram b V2 Double m)) =>
     Double -- ^ side length
  -> P2 Double -- ^ connector start point
  -> P2 Double -- ^ connector endpoint
  -> QDiagram b V2 Double m
generateTriangleEndpoint side_length startpoint endpoint =
  triangle_repositioned
  where
    unmodified_direction = dirBetween startpoint endpoint
    alignment_vector = fromDirection unmodified_direction

    triangle_diagram = triangle side_length
      # rotateTo unmodified_direction
      # rotateBy (1/4)
      # alignBy alignment_vector (-1)
      # lw none

    triangle_repositioned = position [(endpoint, triangle_diagram)]


-- | Uses the midpoint to compute a direction vector to the destination.
--
-- If bidirectional, draws arrowheads on both ends of the connector.
renderArrowTips :: (Monoid m, Semigroup m, TrailLike (QDiagram b V2 Double m)) =>
     Double -- ^ triangle side length
  -> Bool -- ^ is bidirectional
  -> P2 Double -- ^ midpoint
  -> P2 Double -- ^ origin
  -> P2 Double -- ^ destination
  -> QDiagram b V2 Double m -- ^ output diagram
renderArrowTips
    triangle_side_length
    is_bidirectional
    midpoint
    origin_point
    destination_point =

  mconcat $ modifier preliminary_arrowhead_list
  where

    triangle_gen_helper = generateTriangleEndpoint
      triangle_side_length
      midpoint

    origin_triangle = triangle_gen_helper origin_point
    destination_triangle = triangle_gen_helper destination_point

    preliminary_arrowhead_list = [destination_triangle]
    modifier = BasicUtils.consIf is_bidirectional origin_triangle


-- | Draws two copies of the diagram: one with the specified foreground color,
-- and a dropshadow underneath with the specified offset
drawWithShadow :: Semigroup m =>
     Kolor -- ^ foreground color
  -> Kolor -- ^ connector color
  -> V (QDiagram b V2 Double m) (N (QDiagram b V2 Double m))
  -> QDiagram b V2 Double m -- ^ input diagram
  -> QDiagram b V2 Double m -- ^ output diagram
drawWithShadow
  inner_color
  dropshadow_color
  dropshadow_offset_vector
  input_diagram =

  input_diagram # apply_solid_color inner_color
    <> input_diagram
        # apply_solid_color dropshadow_color
        # translate dropshadow_offset_vector
  where
    apply_solid_color x = fc x # lc x


regenerateConnectorProps :: Semigroup m =>
     Subdiagram b V2 Double m
  -> Subdiagram b V2 Double m
  -> Positioning.FixedPointConnection a b1 m1
  -> Positioning.ConnectorProperties
regenerateConnectorProps origin_diagram destination_diagram fixed_point_connection =

  Positioning.NewConnectorProperties connector_props_inner placement_relation
  where

    origin_attachment_side = Positioning.side $ Positioning.attachment $
      Positioning.fromAttachment fixed_point_connection

    destination_attachment_side = Positioning.side $ Positioning.attachment $
      Positioning.toAttachment fixed_point_connection


    placement_relation = Positioning.getPlacementRelations origin_diagram destination_diagram

    connector_props_inner = Positioning.NewBaseConnectorProps
      origin_attachment_side
      destination_attachment_side
      (Positioning.connectionMidpointFunc fixed_point_connection)


-- | Determines the endpoints and midpoint of the connector, given the origin
-- and destination diagrams.
--
-- Places arrowheads on one or both ends of the connector.
renderConnector :: (Monoid m, Semigroup m, TrailLike (QDiagram b V2 Double m)) =>
     Positioning.FixedPointConnection a b m
  -> Kolor -- ^ connector color
  -> Bool -- ^ is bidirectional
  -> QDiagram b V2 Double m -- ^ output diagram
renderConnector fixed_point_connection inner_color is_bidirectional =

  drawWithShadow
    inner_color
    gray
    dropshadow_offset_vector
    connector_with_arrowheads

  where
    origin_attachment_reference = Positioning.fromAttachment fixed_point_connection
    destination_attachment_reference = Positioning.toAttachment fixed_point_connection

    get_diagram = Positioning.subdiagram . Positioning.subdiagramReference . Positioning.attachment
    origin_diagram = get_diagram origin_attachment_reference
    destination_diagram = get_diagram destination_attachment_reference

    dropshadow_offset_distance = Style.triangleSideLength / 3
    dropshadow_offset_vector = scale dropshadow_offset_distance $ angleV $ -45 @@ deg

    connector_with_arrowheads = foreground_connector <> arrowheads

    connector_props_new = regenerateConnectorProps
      origin_diagram
      destination_diagram
      fixed_point_connection

    positioned_points = Positioning.computeConnectorPoints
      origin_diagram
      destination_diagram
      connector_props_new

    origin_point = Positioning.orig $ Positioning.segment positioned_points
    destination_point = Positioning.dest $ Positioning.segment positioned_points
    midpoint = Positioning.midpoint positioned_points

    arrowheads = renderArrowTips
      Style.triangleSideLength
      is_bidirectional
      midpoint
      origin_point
      destination_point

    foreground_connector = connector_trail # lwL Style.baseLineWeight


    triangle_height = Style.triangleSideLength * sqrt 3 / 2
    connector_standoff_length = triangle_height


    origin_alignment_vector = fromDirection $ dirBetween midpoint origin_point
    destination_alignment_vector = fromDirection $ dirBetween midpoint destination_point

    offset_origin_point = translate
      (scale connector_standoff_length origin_alignment_vector)
      origin_point

    corrected_origin_point = if is_bidirectional then offset_origin_point else origin_point

    offset_destination_point = translate
      (scale connector_standoff_length destination_alignment_vector)
      destination_point

    connector_trail = fromVertices [
        corrected_origin_point
      , midpoint
      , offset_destination_point
      ]


-- | Places arrowheads on one or both ends of the connector.
renderFixedConnector :: (Monoid m, Semigroup m, TrailLike (QDiagram b V2 Double m)) =>
     (Positioning.SubdiagramReference a b m -> Kolor)
  -> Positioning.FixedPointConnection a b m
  -> QDiagram b V2 Double m
  -> QDiagram b V2 Double m
renderFixedConnector
    color_provider
    fixed_point_connection
    d =

  d <> additions
  where
    origin_subdiagram_reference = Positioning.subdiagramReference $
      Positioning.attachment $ Positioning.fromAttachment fixed_point_connection

    inner_color = color_provider origin_subdiagram_reference

    is_bidirectional = Positioning.bidirectionalAttachment fixed_point_connection

    additions = renderConnector
      fixed_point_connection
      inner_color
      is_bidirectional
