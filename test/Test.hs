module Main (main) where

import Hedgehog (Group, checkParallel)
import qualified REYES.SamplerTest (tests)
import Test.DocTest (doctest)

main :: IO ()
main = do
  runHedgehogTests
  runDocTests

runHedgehogTests :: IO ()
runHedgehogTests = do
  putStrLn "\n---- Running Hedgehog Tests ----"
  mapM_ checkParallel hedgehogTests
  putStrLn "---- Finished Hedgehog Tests ----"

hedgehogTests :: [Group]
hedgehogTests =
  [REYES.SamplerTest.tests]

runDocTests :: IO ()
runDocTests = do
  putStrLn "\n---- Running Doctests ----"
  docTests
  putStrLn "---- Finished Doctests ----"

docTests :: IO ()
docTests =
  doctest
    [ "-isrc",
      "src/REYES/Sampler.hs"
    ]
