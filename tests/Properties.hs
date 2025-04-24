import GeneratedTests
import Proto3.Suite.Haskell.Parser (initLogger)
import Test.Tasty

main :: IO ()
main = do
  logger <- initLogger
  defaultMain $ testGroup "GRPC Unit Tests" [generatedTests logger]
