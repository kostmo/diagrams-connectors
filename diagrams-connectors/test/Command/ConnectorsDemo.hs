-- | Experimental HTML rendering of DBC content.
-- This is one of multiple "frontends", besides the DBC renderer and .compact.json renderer.
module Command.ConnectorsDemo where

import qualified Data.HashMap.Strict                              as HashMap
import           Data.List                                        (intersperse)
import qualified Data.Set                                         as Set
import           Data.Text                                        (Text)
import qualified Data.Text                                        as T
import qualified Diagrams.Backend.SVG                             as Svg
import           Diagrams.Prelude                                 as DP
import           System.Random                                    (mkStdGen,
                                                                   randomR)

import qualified Diagrams.TwoD.OrthoConnect.BasicUtils            as BasicUtils
import qualified Diagrams.TwoD.OrthoConnect.ColorUtils            as ColorUtils
import qualified Diagrams.TwoD.OrthoConnect.Connectivity          as Connectivity
import qualified Diagrams.TwoD.OrthoConnect.Positioning           as Positioning
import qualified Diagrams.TwoD.OrthoConnect.RightAngleConnectors  as RightAngleConnectors
import qualified Diagrams.TwoD.OrthoConnect.StraightBusConnectors as StraightBusConnectors


instance IsName Text


data SimpleScenario = SimpleA | SimpleB | SimpleC
  deriving (Bounded, Enum, Show)


data ComplexScenario = ComplexA | ComplexB | ComplexC | ComplexD
  deriving (Bounded, Enum, Show)


data OverlapScenario = OverlapHorizontal | OverlapVertical
  deriving (Bounded, Enum, Show)


data StraightBusScenario = EvenNodesStraightBus | OddNodesStraightBus | RandomizedStraightBus
  deriving (Bounded, Enum, Eq, Show)


data WidthHeight = NewWidthHeight Double Double


smallBoxSize = NewWidthHeight 6 1.5


textSizedBox ::
     WidthHeight
  -> Text -- ^ box name
  -> DP.Diagram Svg.B
textSizedBox (NewWidthHeight width height) box_name =

  text (T.unpack box_name)
    <> rect width height # fc kolor . lw line_weight # named diagram_label

  where
    diagram_label = box_name
    line_weight = 1
    kolor = DP.gray


singleNamedBox ::
     Text -- ^ box name
  -> DP.Diagram Svg.B
singleNamedBox = textSizedBox smallBoxSize


renderConnectedNodesSimple ::
     (Double -> [Diagram Svg.B] -> Diagram Svg.B)
  -> [Text]
  -> [Connectivity.BoxNameConnection Text]
  -> Diagram Svg.B
renderConnectedNodesSimple separator_function node_list =
  renderConnectedDiagram intermediate_diagram node_list
  where
    intermediate_diagram = separator_function 2 $ map singleNamedBox node_list


renderOverlapScenario ::
     OverlapScenario
  -> Diagram Svg.B
renderOverlapScenario scenario =

  renderConnectedDiagram intermediate_diagram node_list [connection]

  where
    scenario_name = show scenario

    box_width = 8
    box_height = 3
    big_box = textSizedBox $ NewWidthHeight box_width box_height

    intermediate_diagram = case scenario of
      OverlapHorizontal -> big_box name1 ||| strutX (box_width / 2) ||| centerY (big_box name2 === strutY box_height)
      OverlapVertical -> big_box name1 === strutY box_height === centerX (big_box name2 ||| strutX box_width)

    name1 = T.pack $ scenario_name ++ show 1
    name2 = T.pack $ scenario_name ++ show 2

    connection = Connectivity.NewBoxNameConnection name1 name2

    node_list = [
        name1
      , name2
      ]


renderSimpleScenario ::
     (Double -> [Diagram Svg.B] -> Diagram Svg.B)
  -> SimpleScenario
  -> Diagram Svg.B
renderSimpleScenario separator_function scenario =
  renderConnectedNodesSimple separator_function node_list connection_list
  where

    name1 = T.pack $ scenario_name ++ show 1
    name2 = T.pack $ scenario_name ++ show 2

    scenario_name = show scenario

    node_list = [
        name1
      , name2
      ]

    forward = Connectivity.NewBoxNameConnection name1 name2
    backward = Connectivity.NewBoxNameConnection name2 name1

    connection_list = case scenario of
      SimpleA -> [forward]
      SimpleB -> [backward]
      SimpleC -> [forward, backward]


renderConnectedDiagram ::
     Diagram Svg.B -- ^ intermediate diagram
  -> [Text] -- ^ node list
  -> [Connectivity.BoxNameConnection Text]
  -> Diagram Svg.B -- ^ connected diagram
renderConnectedDiagram intermediate_diagram node_list connection_list =

  foldr
    (RightAngleConnectors.renderFixedConnector provide_color)
    intermediate_diagram
    fixed_connection_positions

  where
    fixed_connection_positions = Positioning.obtainPositionedConnections
      False
      intermediate_diagram
      connection_list

    unique_nodes = Set.fromList node_list
    node_color_map = ColorUtils.generateNodeColorMap unique_nodes

    provide_color x = HashMap.lookupDefault DP.black node_name node_color_map
      where
        node_name = Positioning.name x


renderConnectedNodesComplex ::
     Text
  -> [Text]
  -> [Diagram Svg.B]
  -> Bool
  -> [Connectivity.BoxNameConnection Text]
  -> Diagram Svg.B
renderConnectedNodesComplex origin_node destination_nodes items is_flipped =
  renderConnectedDiagram intermediate_diagram node_list
  where
    node_list = origin_node : destination_nodes
    intermediate_diagram = hcat $ BasicUtils.transformIf is_flipped reverse items


renderComplexScenario :: Bool -> ComplexScenario -> Diagram Svg.B
renderComplexScenario is_flipped scenario = case scenario of
  ComplexA -> f standard_connections items1
  ComplexB -> f standard_connections [
      singleNamedBox origin_node
    , strutX 3
    , vsep 4 $ map singleNamedBox destination_nodes
    ]
  ComplexC -> f standard_connections [
      singleNamedBox origin_node
    , strutX 3
    , strutY 3 === vsep 4 (map singleNamedBox destination_nodes)
    ]
  ComplexD -> f (Connectivity.NewBoxNameConnection name3 name2 : standard_connections) items1

  where
    items1 = [
        centerY $ singleNamedBox origin_node
      , strutX 3
      , centerY $ vsep 4 $ map singleNamedBox destination_nodes
      ]


    f conns items = renderConnectedNodesComplex origin_node destination_nodes items is_flipped conns

    standard_connections = [
        Connectivity.NewBoxNameConnection name1 name2
      , Connectivity.NewBoxNameConnection name1 name3
      ]

    scenario_name = show scenario
    name1 = T.pack $ scenario_name ++ show 1
    name2 = T.pack $ scenario_name ++ show 2
    name3 = T.pack $ scenario_name ++ show 3

    origin_node = name1
    destination_nodes = [name2, name3]


renderRawSimple ::
  Diagram Svg.B
renderRawSimple =
  frame 1 simple_diagrams

  where
    mk_array x = map (renderSimpleScenario x) BasicUtils.allEnumValues
    simple_diagrams_horizontal = mk_array hsep
    simple_diagrams_vertical = mk_array vsep

    simple_diagrams = vsep 3 $ map centerX $
      simple_diagrams_horizontal ++ [hsep 3 simple_diagrams_vertical]


renderRawOverlaps ::
  Diagram Svg.B
renderRawOverlaps =
  frame 1 overlap_diagrams
  where
    overlap_diagrams = vsep 3 $ map (centerX . renderOverlapScenario) BasicUtils.allEnumValues


renderStraightBusScenarios ::
  Diagram Svg.B
renderStraightBusScenarios =
  frame 1 overlap_diagrams
  where
    overlap_diagrams = vsep 5 $ map (centerX . renderStraightBus) BasicUtils.allEnumValues


renderStraightBus ::
     StraightBusScenario
  -> Diagram Svg.B
renderStraightBus scenario_type =

  StraightBusConnectors.renderStraightBusConnector
    StraightBusConnectors.Average
    node_list
    intermediate_diagram

  where
    box_width = 3
    box_height = 2
    little_box = textSizedBox $ NewWidthHeight box_width box_height


    random_generator = mkStdGen 0

    (random_row_count, _new_generator1) = randomR (2 :: Int, 6) random_generator

    is_randomized = scenario_type == RandomizedStraightBus
    row_element_count = if is_randomized
      then random_row_count
      else 5

    node_name_suffixes = map show [0 .. (row_element_count - 1)]

    gen_row prefix = map (\x -> T.pack $ prefix : x) node_name_suffixes
    first_row = gen_row 'a'

    second_row_raw = gen_row 'b'

    second_row = BasicUtils.transformIf
      (scenario_type == OddNodesStraightBus)
      (drop 1)
      second_row_raw

    node_row_diagram node_names = centerX $ hsep 2 $
      intersperse (strutX $ box_width / 2) $ map little_box node_names

    intermediate_diagram = vsep 4 $ map node_row_diagram [first_row, second_row]

    node_list = first_row ++ second_row


renderRawComplex ::
  Diagram Svg.B
renderRawComplex =
  frame 1 columns
  where
    mk_column is_flipped = vsep 3 $ map (renderComplexScenario is_flipped) BasicUtils.allEnumValues
    columns = hsep 3 $ map mk_column BasicUtils.allEnumValues
