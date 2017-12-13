import           Control.Monad (unless)

import           System.IO (BufferMode (..), hSetBuffering, stdout, stderr)
import           System.Exit (exitFailure)

import qualified Test.Play.Random

main :: IO ()
main = runTests
  [ Test.Play.Random.tests
  ]

runTests :: [IO Bool] -> IO ()
runTests tests = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence tests
  unless (and results)
    exitFailure
