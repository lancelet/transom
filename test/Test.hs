module Main (main) where

import Hedgehog (Group, checkParallel)

main :: IO ()
main = runHedgehogTests

runHedgehogTests :: IO ()
runHedgehogTests = do
  putStrLn "\n---- Running Hedgehog Tests ----"
  mapM_ checkParallel hedgehogTests
  putStrLn "---- Finished Hedgehog Tests ----"

hedgehogTests :: [Group]
hedgehogTests = []
