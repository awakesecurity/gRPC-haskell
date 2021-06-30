{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Network.GRPC.LowLevel.GRPC.MetadataMap where

import Data.ByteString (ByteString)
import Data.Function (on)
import GHC.Exts (IsList(..))
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M

-- | Represents metadata for a given RPC, consisting of key-value pairs. Keys
-- are allowed to be repeated, with the `last` element of value list usually
-- taken as the final value for that key. Since repeated keys are unlikely in
-- practice, the 'IsList' instance uses key-value pairs as items. For example,
-- @fromList [("key1","val1"),("key2","val2"),("key1","val3")]@.
newtype MetadataMap = MetadataMap {unMap :: M.Map ByteString [ByteString]}
  deriving Eq

instance Show MetadataMap where
  show m = "fromList " ++ show (M.toList (unMap m))

instance Semigroup MetadataMap where
  MetadataMap m1 <> MetadataMap m2 =
    MetadataMap $ M.unionWith (<>) m1 m2

instance Monoid MetadataMap where
  mempty = MetadataMap M.empty

instance IsList MetadataMap where
  type Item MetadataMap = (ByteString, ByteString)
  fromList = MetadataMap
             . M.fromList
             . map (\xs -> ((fst . head) xs, map snd xs))
             . groupBy ((==) `on` fst)
             . sortBy (comparing fst)
  toList = concatMap (\(k,vs) -> map (k,) vs)
           . map (fmap toList)
           . M.toList
           . unMap
