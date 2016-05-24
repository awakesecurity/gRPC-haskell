import           LowLevelTests
import           Test.Tasty
import           UnsafeTests

main :: IO ()
main = defaultMain $ testGroup "GRPC Unit Tests"
  [ unsafeTests
  , lowLevelTests
  ]
