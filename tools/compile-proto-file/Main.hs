{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Monoid                          ((<>))
import           Data.Protobuf.Wire.DotProto.Generate
import           Filesystem.Path.CurrentOS            (encodeString)
import           Options.Generic
import           Prelude                              hiding (FilePath)
import           Turtle                               (FilePath)

data Args = Args
  { proto :: FilePath <?> "Path to input .proto file"
  } deriving (Generic, Show)
instance ParseRecord Args

main :: IO ()
main = do
  protoPath <- encodeString . unHelpful . proto <$> getRecord "Dumps a compiled .proto file to stdout"
  readDotProtoWithContext protoPath >>= \case
    Left err        -> fail (show err)
    Right (dp, ctx) -> case renderHsModuleForDotProto dp ctx of
      Left err  -> fail ("Error compiling " <> protoPath <> ": " <> show err)
      Right src -> putStrLn src
