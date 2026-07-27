import LowLevelTests
import LowLevelTests.Op
import Test.Tasty
import Test.Tasty.Runners (NumThreads (..))
import UnsafeTests

main :: IO ()
main =
  defaultMain $
    localOption (NumThreads 1) $
      testGroup
        "GRPC Unit Tests"
        [ unsafeTests
        , unsafeProperties
        , lowLevelOpTests
        , lowLevelTests
        ]
