module Diagrams.TwoD.OrthoConnect.ColorUtils where

import qualified Data.Colour.Palette.Harmony as Harmony
import           Data.Hashable               (Hashable)
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import qualified Diagrams.Prelude            as DP
import           System.Random               (mkStdGen)
import           System.Random.Shuffle       (shuffle')


generateNodeColorMap :: (Eq a, Hashable a) => Set a -> HashMap a Harmony.Kolor
generateNodeColorMap all_node_names =
  HashMap.fromList $ zip sorted_unique_nodes $ cycle shuffled_colors
  where
    sorted_unique_nodes = Set.toAscList all_node_names
    shuffled_colors = generateColorSequence $ length sorted_unique_nodes


-- | TODO Another option may be to use 'infiniteWebColors' and filter out all
-- colors darker than a certain value.
generateColorSequence ::
     Int -- ^ item count
  -> [Harmony.Kolor]
generateColorSequence item_count =
  shuffled_colors
  where
    brightness_level_count = 3
    base_colors = [Harmony.tint (fromIntegral j / fromIntegral brightness_level_count) DP.red | j <- [0..(brightness_level_count - 1)]]
    level_color_count = ceiling $ fromIntegral item_count / fromIntegral brightness_level_count
    ordered_colors = concat [[Harmony.rotateColor (fromIntegral i * 360 / fromIntegral level_color_count) base_color | i <- [0 .. (level_color_count - 1)]] | base_color <- base_colors]

    shuffled_colors = shuffle' ordered_colors (length ordered_colors) (mkStdGen 0)
