import GeneratedTests
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "GRPC Unit Tests" [generatedTests]
