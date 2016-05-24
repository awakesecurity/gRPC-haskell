module Network.GRPC.Unsafe.Metadata where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString, useAsCString, packCString)
import Data.Map.Strict as M
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable


#include <grpc/grpc.h>
#include <grpc/status.h>
#include <grpc/impl/codegen/grpc_types.h>
#include <grpc_haskell.h>

-- | Represents a pointer to one or more metadata key/value pairs. This type
-- is intended to be used when sending metadata.
{#pointer *grpc_metadata as MetadataKeyValPtr newtype#}

-- | Represents a pointer to a grpc_metadata_array. Must be destroyed with
-- 'metadataArrayDestroy'. This type is intended for receiving metadata.
-- This can be populated by passing it to e.g. 'grpcServerRequestCall'.
-- TODO: we need a function for getting a 'MetadataKeyValPtr'
-- and length from this type.
{#pointer *grpc_metadata_array as MetadataArray newtype#}

{#fun metadata_array_get_metadata as ^
  {`MetadataArray'} -> `MetadataKeyValPtr'#}

{#fun metadata_array_get_count as ^ {`MetadataArray'} -> `Int'#}

instance Storable MetadataArray where
  sizeOf (MetadataArray r) = sizeOf r
  alignment (MetadataArray r) = alignment r
  peek p = fmap MetadataArray (peek (castPtr p))
  poke p (MetadataArray r) = poke (castPtr p) r

-- | Create an empty 'MetadataArray'. Returns a pointer to it so that we can
-- pass it to the appropriate op creation functions.
{#fun metadata_array_create as ^ {} -> `Ptr MetadataArray' id#}

{#fun metadata_array_destroy as ^ {id `Ptr MetadataArray'} -> `()'#}

-- Note: I'm pretty sure we must call out to C to allocate these
-- because they are nested structs.
-- | Allocates space for exactly n metadata key/value pairs.
{#fun metadata_alloc as ^ {`Int'} -> `MetadataKeyValPtr'#}

{#fun metadata_free as ^ {`MetadataKeyValPtr'} -> `()'#}

-- | Sets a metadata key/value pair at the given index in the
-- 'MetadataKeyValPtr'. No error checking is performed to ensure the index is
-- in bounds!
{#fun set_metadata_key_val as setMetadataKeyVal
  {useAsCString* `ByteString', useAsCString* `ByteString',
   `MetadataKeyValPtr', `Int'} -> `()'#}

{#fun get_metadata_key as getMetadataKey'
  {`MetadataKeyValPtr', `Int'} -> `CString'#}

{#fun get_metadata_val as getMetadataVal'
  {`MetadataKeyValPtr', `Int'} -> `CString'#}

--TODO: The test suggests this is leaking.
withMetadataArrayPtr :: (Ptr MetadataArray -> IO a) -> IO a
withMetadataArrayPtr = bracket metadataArrayCreate metadataArrayDestroy

withMetadataKeyValPtr :: Int -> (MetadataKeyValPtr -> IO a) -> IO a
withMetadataKeyValPtr i f = bracket (metadataAlloc i) metadataFree f

getMetadataKey :: MetadataKeyValPtr -> Int -> IO ByteString
getMetadataKey m = getMetadataKey' m >=> packCString

getMetadataVal :: MetadataKeyValPtr -> Int -> IO ByteString
getMetadataVal m = getMetadataVal' m >=> packCString

createMetadata :: M.Map ByteString ByteString -> IO MetadataKeyValPtr
createMetadata m = do
  let l = M.size m
  let indexedKeyVals = zip [0..] $ M.toList m
  metadata <- metadataAlloc l
  forM_ indexedKeyVals $ \(i,(k,v)) -> setMetadataKeyVal k v metadata i
  return metadata

getAllMetadataArray :: MetadataArray -> IO (M.Map ByteString ByteString)
getAllMetadataArray m = do
  kvs <- metadataArrayGetMetadata m
  l <- metadataArrayGetCount m
  getAllMetadata kvs l

getAllMetadata :: MetadataKeyValPtr -> Int -> IO (M.Map ByteString ByteString)
getAllMetadata m count = do
  let indices = [0..count-1]
  fmap M.fromList $ forM indices $
    \i -> liftM2 (,) (getMetadataKey m i) (getMetadataVal m i)
