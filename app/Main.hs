module Main where

import           Hebrew

main :: IO ()
main = putStrLn $ show $ (prettyRead "שלם" :: HFWord)
