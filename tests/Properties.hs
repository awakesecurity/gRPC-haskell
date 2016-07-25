import           LowLevelTests
import           LowLevelTests.Op
import           Test.Tasty
import           UnsafeTests
import           GeneratedTests

main :: IO ()
main = defaultMain $ testGroup "GRPC Unit Tests"
  [ unsafeTests
  , lowLevelOpTests
  , lowLevelTests
  , generatedTests
  ]
