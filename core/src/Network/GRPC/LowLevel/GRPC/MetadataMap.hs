{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Network.GRPC.LowLevel.GRPC.MetadataMap where

import Data.ByteString (ByteString)
import Data.Function (on)
import GHC.Exts (IsList(..))
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M

{- | Represents metadata for a given RPC, consisting of key-value pairs (often
   referred to as "GRPC custom metadata headers").

   Keys are allowed to be repeated, with the 'last' value element (i.e., the
   last-presented) usually taken as the value for that key (see 'lookupLast' and
   'lookupAll').

   Since repeated keys are unlikely in practice, the 'IsList' instance for
   'MetadataMap' uses key-value pairs as items, and treats duplicates
   appropriately.

   >>> lookupAll "k1" (fromList [("k1","x"), ("k2", "z"), ("k1", "y")])
   Just ("x" :| ["y"])

   >>> lookupLast "k1" (fromList [("k1","x"), ("k2", "z"), ("k1", "y")])
   Just "y"
-}

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

-- | Obtain all header values for a given header key, in presentation order.
lookupAll :: ByteString -> MetadataMap -> Maybe (NE.NonEmpty ByteString)
lookupAll k (MetadataMap md) = NE.nonEmpty =<< M.lookup k md

-- | Obtain the last-presented header value for a given header key.
lookupLast :: ByteString -> MetadataMap -> Maybe ByteString
lookupLast k = fmap NE.last . lookupAll k
