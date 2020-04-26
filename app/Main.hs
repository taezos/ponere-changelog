module Main where

import           UmuChangelog

main :: IO ()
main = print =<< getLatestTag
