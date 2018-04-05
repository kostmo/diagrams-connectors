module Diagrams.TwoD.OrthoConnect.BasicUtils where

import qualified Data.Foldable       as F
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap


-- | AKA conditional prepend.
-- If the conditional (first argument) is true, returns a function that
-- prepends the second argument to the function's argument.
-- Otherwise, returns the 'id' function.
consIf :: Bool -> a -> [a] -> [a]
consIf conditional prefix = transformIf conditional (prefix:)


-- | If the conditional (first argument) is true, returns the given
-- transformation (second argument).  Othewise return the identity function.
--
-- Compare to the
-- <https://hackage.haskell.org/package/cond-0.1/docs/Control-Cond.html#v:-63-. conditional composition operator>
-- in the @\"cond\"@ package.
transformIf :: Bool -> (a -> a) -> (a -> a)
transformIf conditional f = if conditional then f else id


hasMultiple :: Foldable f => f a -> Bool
hasMultiple = (> 1) . F.length


-- | Compare to <http://stackoverflow.com/a/12398993/105137 this StackOverflow solution>.
binTuplesByFirst :: (Hashable k, Eq k) => [(k, v)] -> HashMap k [v]
binTuplesByFirst = foldr f HashMap.empty
  where
    f t = HashMap.insertWith (++) (fst t) [snd t]


-- | Accepts a key extractor function, using the original item as the second
-- tuple element.
deriveTupleKeys :: (a -> b) -> [a] -> [(b, a)]
deriveTupleKeys f = map (\x -> (f x, x))


binDerivedTuples x = binTuplesByFirst . deriveTupleKeys x


allEnumValues :: (Bounded a, Enum a) => [a]
allEnumValues = [minBound..]
