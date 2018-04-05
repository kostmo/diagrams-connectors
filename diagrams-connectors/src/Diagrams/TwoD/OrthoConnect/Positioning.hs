{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module Diagrams.TwoD.OrthoConnect.Positioning (
    -- * Main functions
    obtainPositionedConnections
  , computeConnectorPoints
  , getPlacementRelations
    -- * Field accessors
  , attachment
  , side
  , segment
  , midpoint
  , subdiagramReference
  , orig
  , dest
  , getBoxDiagramAttachment
  , pointFromBoxAttachment
  , SubdiagramReference (..)
  , FixedPointConnection (..)
  , BaseConnectorProperties (NewBaseConnectorProps)
  , ConnectorProperties (NewConnectorProperties)
  ) where

import           Data.Function                           (on)
import           Data.Hashable                           (Hashable)
import           Data.List                               (partition)
import           Data.Map                                (fromListWith, toList)
import qualified Data.Maybe                              as Maybe
import           Data.Ord                                (comparing)
import           Diagrams.Prelude                        hiding (from, to)

import qualified Diagrams.TwoD.OrthoConnect.Connectivity as Connectivity
import qualified Diagrams.TwoD.OrthoConnect.Coords       as Coords


class RectBoundable a where
  leftBound :: a -> Coords.Point2D
  rightBound :: a -> Coords.Point2D
  topBound :: a -> Coords.Point2D
  bottomBound :: a -> Coords.Point2D

instance Semigroup m1 => RectBoundable (Subdiagram b1 V2 Double m1) where
  leftBound x = boundaryFrom x unit_X
  rightBound x = boundaryFrom x unitX
  topBound x = boundaryFrom x unitY
  bottomBound x = boundaryFrom x unit_Y

instance RectBoundable Coords.Point2D where
  leftBound = id
  rightBound = id
  topBound = id
  bottomBound = id


data SubdiagramReference a b m = SubdiagramReference {
    name       :: a
  , subdiagram :: Subdiagram b V2 Double m
  }

instance Eq a => Eq (SubdiagramReference a b m) where
  (==) = (==) `on` name

instance Ord a => Ord (SubdiagramReference a b m) where
  compare = comparing name

instance Show a => Show (SubdiagramReference a b m) where
  show x = show $ name x


data BoxConnection a b m = NewBoxConnection {
    from :: SubdiagramReference a b m
  , to   :: SubdiagramReference a b m
  } deriving Show


-- | Used for grouping symmetric connections
data DirectionalBoxConnection a b m = DirectionalBoxConnection {
    bidirectional :: Bool
  , connection    :: BoxConnection a b m
  } deriving Show


data AttachmentSide = AttachRight | AttachTop | AttachLeft | AttachBottom
  deriving (Eq, Ord, Show)


data SegmentEnd = SegmentOrigin | SegmentDestination
  deriving Eq


data BoxAttachment a b m = NewBoxAttachment {
    subdiagramReference :: SubdiagramReference a b m
  , side                :: AttachmentSide
  } deriving (Eq, Ord)


getBoxDiagramAttachment :: BoxAttachment a b m -> BoxDiagramAttachment a b m
getBoxDiagramAttachment a = NewBoxDiagramAttachment (subdiagram $ subdiagramReference a) $ side a


data BoxDiagramAttachment a b m = NewBoxDiagramAttachment {
    attachedDiagram :: Subdiagram b V2 Double m
  , diagramSide     :: AttachmentSide
  }


-- | Operates on rectangular diagrams
--
-- Used to collect all attachments to a certain side of a box
-- so that they may be distributed evenly along the side.
data SidedBoxConnection a b m = NewSidedBoxConnection {
    originAttachment      :: BoxAttachment a b m
  , destinationAttachment :: BoxAttachment a b m
  , isBidirectional       :: Bool
  , midpointFunc          :: DirectedSegment -> P2 Double
  }


data NumberedSidedBoxConnection a b m = NewNumberedSidedBoxConnection {
    sidedBoxConnection :: SidedBoxConnection a b m
  , connectionIndex    :: Int
  }


data FloatingSideConnection a b m = NewFloatingSideConnection {
    whichEnd      :: SegmentEnd
  , forConnection :: SidedBoxConnection a b m
  , myIdx         :: Int
  }


data FixedSideConnection a b m = NewFixedSideConnection {
    whichFixedEnd   :: SegmentEnd
  , fixedAttachment :: FixedPointAttachment a b m
  , connIdx         :: Int
  }


data FixedPointAttachment a b m = NewFixedPointAttachment {
    _sideConnectionIndex :: Int
  , _sideConnectionCount :: Int
  , attachment          :: BoxAttachment a b m
  }


data FixedPointConnection a b m = NewFixedPointConnection {
    fromAttachment          :: FixedPointAttachment a b m
  , toAttachment            :: FixedPointAttachment a b m
  , connectionMidpointFunc  :: DirectedSegment -> P2 Double
  , bidirectionalAttachment :: Bool
  }


-- TODO not used yet
--data RotationalOrientation = Clockwise | CounterClockwise


data HorizontalRelation = HorizontalOverlap | LeftOf | RightOf
  deriving (Eq, Ord)


data VerticalRelation = VerticalOverlap | Above | Below
  deriving (Eq, Ord)


data DirectedSegment = NewDirectedSegment {
    orig :: Coords.Point2D
  , dest :: Coords.Point2D
  }


data ConnectorPoints = ConnectorPoints {
    segment  :: DirectedSegment
  , midpoint :: Coords.Point2D
  }


data BaseConnectorProperties = NewBaseConnectorProps {
    _originAttachmentSide      :: AttachmentSide
  , _destinationAttachmentSide :: AttachmentSide
  , _connectorFunction         :: DirectedSegment -> P2 Double
  }


data PlacementRelation = NewPlacementRelation {
    horizontalRelation :: HorizontalRelation
  , verticalRelation   :: VerticalRelation
  }


data ConnectorProperties = NewConnectorProperties {
    _baseConnectorProps :: BaseConnectorProperties
  , _placementRelation  :: PlacementRelation
  }


getVectorFromAttachmentSide = \case
  AttachRight  -> unitX
  AttachTop    -> unitY
  AttachLeft   -> unit_X
  AttachBottom -> unit_Y


obtainPositionedConnections :: (Hashable a, IsName a, Semigroup m) =>
     Bool -- ^ force convex connectors
  -> QDiagram b V2 Double m
  -> [Connectivity.BoxNameConnection a]
  -> [FixedPointConnection a b m]
obtainPositionedConnections convex_connectors diagram conns =

  distributeConnectionPositions sided_connections
  where

    consolidated_connections = Connectivity.consolidateConnections conns
    converted_connections = Maybe.mapMaybe (convertWithSubdiagramLookup diagram) consolidated_connections

    sided_connections = map (assignConnectionSide convex_connectors) converted_connections


convertWithSubdiagramLookup :: (IsName a, Semigroup m) =>
     QDiagram b V2 Double m
  -> Connectivity.ConsolidatedNamedBoxConnection a
  -> Maybe (DirectionalBoxConnection a b m)
convertWithSubdiagramLookup d named_connection = do
    origin_subdiagram <- lookupName name1 d
    destination_subdiagram <- lookupName name2 d
    return $ DirectionalBoxConnection
      (Connectivity.bidirectional named_connection) $ NewBoxConnection
        (SubdiagramReference name1 origin_subdiagram)
        (SubdiagramReference name2 destination_subdiagram)

  where
    connection_info = Connectivity.connection named_connection
    name1 = Connectivity.from connection_info
    name2 = Connectivity.to connection_info


assignConnectionSide :: (Ord a, Semigroup m) =>
     Bool -- ^ force convex connectors
  -> DirectionalBoxConnection a b m
  -> SidedBoxConnection a b m
assignConnectionSide convex_connectors box_connection =
  NewSidedBoxConnection x1 x2 is_bidirectional midpoint_func
  where
    x1 = NewBoxAttachment origin_subdiagram_reference origin_attachment_side
    x2 = NewBoxAttachment destination_subdiagram_reference destination_attachment_side

    orig_box_connection = connection box_connection

    origin_subdiagram_reference = from orig_box_connection
    destination_subdiagram_reference = to orig_box_connection

    origin_subdiagram = subdiagram origin_subdiagram_reference
    destination_subdiagram = subdiagram destination_subdiagram_reference


    NewConnectorProperties inner_props _placement_relation = getPointPositions
      convex_connectors
      origin_subdiagram
      destination_subdiagram

    NewBaseConnectorProps origin_attachment_side destination_attachment_side midpoint_func = inner_props

    is_bidirectional = bidirectional box_connection


-- | See http://stackoverflow.com/a/12398993/105137
sortAndGroup assocs = fromListWith (++) [(k, [v]) | (k, v) <- assocs]


groupByUnsorted key_extractor vals =
  map snd $ toList $ sortAndGroup $ map extract_key_value_tuple vals
  where
    extract_key_value_tuple x = (key_extractor x, x)


assignPositionsForGroup ::
     [FloatingSideConnection a b m]
  -> [FixedSideConnection a b m]
assignPositionsForGroup floating_connections =
  zipWith convert_to_fixed floating_connections [1 ..]

  where
    group_member_count = length floating_connections

    convert_to_fixed floating_connection idx = NewFixedSideConnection
      which_end
      (NewFixedPointAttachment idx group_member_count box_attachment)
      (myIdx floating_connection)
      where
        which_end = whichEnd floating_connection
        for_connection = forConnection floating_connection

        box_attachment = case which_end of
          SegmentOrigin      -> originAttachment for_connection
          SegmentDestination -> destinationAttachment for_connection


-- | groups connections by sides, then distributes connections uniformly over
-- each side
distributeConnectionPositions :: (Ord a, Semigroup m) =>
     [SidedBoxConnection a b m]
  -> [FixedPointConnection a b m]
distributeConnectionPositions sided_connections =

  map (reassembleFixedPointConnection fixed_endpoints) numbered_connections
  where
    generate_ends x = [
        NewFloatingSideConnection SegmentOrigin conn idx
      , NewFloatingSideConnection SegmentDestination conn idx
      ]
      where
        conn = sidedBoxConnection x
        idx = connectionIndex x

    -- XXX Here we assign unique IDs to each connection so that we can
    -- rejoin the two endpoints after they are dispersed into groupings
    -- by rectangle side.
    numbered_connections = zipWith NewNumberedSidedBoxConnection sided_connections [0 ..]
    floating_ends = concatMap generate_ends numbered_connections

    extract_key x = (name $ subdiagramReference attachment_end, side attachment_end)
      where
        the_connection = forConnection x
        attachment_end = case whichEnd x of
          SegmentOrigin      -> originAttachment the_connection
          SegmentDestination -> destinationAttachment the_connection

    grouped_ends = groupByUnsorted extract_key floating_ends
    fixed_endpoints = concatMap assignPositionsForGroup grouped_ends


-- | Extracts the origin and destination from a two-element list of
-- 'FixedSideConnection' records to form a single 'FixedPointConnection'.
reassembleFixedPointConnection ::
     [FixedSideConnection a b m]
  -> NumberedSidedBoxConnection a b m
  -> FixedPointConnection a b m
reassembleFixedPointConnection conns numbered_sided_connection =

  NewFixedPointConnection
    (fixedAttachment origin_point)
    (fixedAttachment destination_point)
    (midpointFunc sided_box_connection)
    (isBidirectional sided_box_connection)

  where
    sided_box_connection = sidedBoxConnection numbered_sided_connection
    connection_index = connectionIndex numbered_sided_connection

    -- TODO Should probably use a hashmap here
    relevant_fixed_side_connections = filter ((== connection_index) . connIdx) conns

    (origin_points, destination_points) = partition
      ((== SegmentOrigin) . whichFixedEnd)
      relevant_fixed_side_connections

    origin_point = if length origin_points == 1
      then head origin_points
      else error "Should be exactly one origin point!"

    destination_point = if length destination_points == 1
      then head destination_points
      else error "Should be exactly one destination point!"


pointFromBoxAttachment :: Semigroup m => BoxDiagramAttachment a b m -> Coords.Point2D
pointFromBoxAttachment ba =
  boundaryFrom (attachedDiagram ba) $ getVectorFromAttachmentSide $ diagramSide ba


-- | Derives concrete points from the abstract connector properties
computeConnectorPoints :: Semigroup m =>
     Subdiagram b V2 Double m
  -> Subdiagram b V2 Double m
  -> ConnectorProperties
  -> ConnectorPoints
computeConnectorPoints origin_diagram destination_diagram connector_props =

  ConnectorPoints fully_replaced_directed_segment mid_point
  where

    NewConnectorProperties inner_props placement_relation = connector_props
    NewBaseConnectorProps origin_attachment_side destination_attachment_side midpoint_func = inner_props

    origin_connection_point = pointFromBoxAttachment $ NewBoxDiagramAttachment origin_diagram origin_attachment_side
    destination_connection_point = pointFromBoxAttachment $ NewBoxDiagramAttachment destination_diagram destination_attachment_side

    original_directed_segment = NewDirectedSegment
      origin_connection_point
      destination_connection_point

    mid_point = midpoint_func original_directed_segment

    partially_replaced_directed_segment = if horizontalRelation placement_relation == HorizontalOverlap
      then NewDirectedSegment
        (p2 (Coords.getX mid_point, Coords.getY $ orig original_directed_segment))
        (p2 (Coords.getX mid_point, Coords.getY $ dest original_directed_segment))
      else original_directed_segment

    fully_replaced_directed_segment = if verticalRelation placement_relation == VerticalOverlap
      then NewDirectedSegment
        (p2 (Coords.getX $ orig partially_replaced_directed_segment, Coords.getY mid_point))
        (p2 (Coords.getX $ dest partially_replaced_directed_segment, Coords.getY mid_point))
      else partially_replaced_directed_segment


getPlacementRelations :: RectBoundable a =>
     a
  -> a
  -> PlacementRelation
getPlacementRelations origin_diagram destination_diagram =
  NewPlacementRelation horizontal_relation vertical_relation
  where
    horizontal_relation = getHorizontalRelation origin_diagram destination_diagram
    vertical_relation = getVerticalRelation origin_diagram destination_diagram


getPointPositions :: RectBoundable a =>
     Bool -- ^ force convex connectors
  -> a
  -> a
  -> ConnectorProperties
getPointPositions convex_connectors origin_diagram destination_diagram =

  NewConnectorProperties connector_props placement_relation
  where
    connector_props = getConnectorSides placement_relation convex_connectors
    placement_relation = getPlacementRelations origin_diagram destination_diagram


getConnectorSides ::
     PlacementRelation
  -> Bool -- ^ force convex connectors
  -> BaseConnectorProperties
getConnectorSides placement_relation convex_connectors = case placement_relation of
  NewPlacementRelation LeftOf Above ->            NewBaseConnectorProps AttachBottom AttachLeft  getSquareElbowMidpoint
  -- O
  -- \-D
  NewPlacementRelation LeftOf Below ->            NewBaseConnectorProps AttachTop    AttachLeft  getSquareElbowMidpoint
  -- /-D
  -- O
  NewPlacementRelation LeftOf VerticalOverlap ->  NewBaseConnectorProps AttachRight  AttachLeft  getStraightMidpoint
  -- O-D
  NewPlacementRelation RightOf Above -> if convex_connectors
                           then NewBaseConnectorProps AttachLeft   AttachTop   getSquareElbowMidpoint2
                           else NewBaseConnectorProps AttachBottom AttachRight getSquareElbowMidpoint
  --   O
  -- D-/
  NewPlacementRelation RightOf Below ->           NewBaseConnectorProps AttachTop    AttachRight getSquareElbowMidpoint
  -- D-\
  --   O
  NewPlacementRelation RightOf VerticalOverlap -> NewBaseConnectorProps AttachLeft   AttachRight getStraightMidpoint
  -- D-O
  NewPlacementRelation HorizontalOverlap Above -> NewBaseConnectorProps AttachBottom AttachTop   getStraightMidpoint
  -- O
  -- D
  NewPlacementRelation HorizontalOverlap Below -> NewBaseConnectorProps AttachTop    AttachBottom  getStraightMidpoint
  -- D
  -- O
  NewPlacementRelation HorizontalOverlap VerticalOverlap -> NewBaseConnectorProps AttachTop  AttachBottom  getStraightMidpoint
  -- (O D)


getSquareElbowMidpoint :: DirectedSegment -> P2 Double
getSquareElbowMidpoint (NewDirectedSegment a b) = p2 (Coords.getX a, Coords.getY b)


getSquareElbowMidpoint2 :: DirectedSegment -> P2 Double
getSquareElbowMidpoint2 (NewDirectedSegment a b) = p2 (Coords.getX b, Coords.getY a)


getStraightMidpoint :: DirectedSegment -> P2 Double
getStraightMidpoint (NewDirectedSegment a b) = p2 ((Coords.getX a + Coords.getX b) / 2, (Coords.getY a + Coords.getY b) / 2)


getHorizontalRelation :: RectBoundable a =>
     a -- ^ origin diagram
  -> a -- ^ destination diagram
  -> HorizontalRelation
getHorizontalRelation
    origin_diagram
    destination_diagram
  | horizontal_comparison left_point_a right_point_b = RightOf
  | horizontal_comparison left_point_b right_point_a = LeftOf
  | otherwise = HorizontalOverlap

  where
    left_point_a = leftBound origin_diagram
    right_point_a = rightBound origin_diagram

    left_point_b = leftBound destination_diagram
    right_point_b = rightBound destination_diagram

    horizontal_comparison = (>) `on` Coords.getX


getVerticalRelation :: RectBoundable a =>
     a -- ^ origin diagram
  -> a -- ^ destination diagram
  -> VerticalRelation
getVerticalRelation
    origin_diagram
    destination_diagram
  | vertical_comparison bottom_point_a top_point_b = Above
  | vertical_comparison bottom_point_b top_point_a = Below
  | otherwise = VerticalOverlap

  where
    top_point_a = topBound origin_diagram
    bottom_point_a = bottomBound origin_diagram

    top_point_b = topBound destination_diagram
    bottom_point_b = bottomBound destination_diagram

    vertical_comparison = (>) `on` Coords.getY
