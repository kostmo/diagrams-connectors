module Diagrams.TwoD.OrthoConnect.Connectivity where

import           Data.Hashable                        (Hashable)
import qualified Data.HashMap.Strict                  as HashMap
import           Data.List                            (partition, sort)
import           Data.Tuple                           (swap)

import qualified Diagrams.TwoD.OrthoConnect.BasicUtils as BasicUtils


data BoxNameConnection a = NewBoxNameConnection {
    from :: a
  , to   :: a
  } deriving (Eq, Ord, Show)


getNormalizedKey :: Ord a => BoxNameConnection a -> (a, a)
getNormalizedKey (NewBoxNameConnection f t) =
  BasicUtils.transformIf (t > f) swap (f, t)


-- | Used for grouping symmetric connections
data ConsolidatedNamedBoxConnection a = ConsolidatedNamedBoxConnection {
    bidirectional :: Bool
  , connection    :: BoxNameConnection a
  } deriving (Show, Eq, Ord)


-- * Connection generation

-- | Absorbs two opposite connections into a single bidirectional connection
consolidateConnections :: (Hashable a, Ord a) =>
     [BoxNameConnection a]
  -> [ConsolidatedNamedBoxConnection a]
consolidateConnections connections = unidirectional_connections ++ final_bidirectional_connections
  where

    connections_by_normalized_key = BasicUtils.binDerivedTuples getNormalizedKey connections
    (bidir, unidir) = partition (BasicUtils.hasMultiple . snd) $ HashMap.toList connections_by_normalized_key

    bidir_keys = map fst bidir
    unidir_vals = concatMap snd unidir

    unidirectional_connections = sort $ map (ConsolidatedNamedBoxConnection False) unidir_vals

    f = ConsolidatedNamedBoxConnection True . uncurry NewBoxNameConnection
    final_bidirectional_connections = sort $ map f bidir_keys
