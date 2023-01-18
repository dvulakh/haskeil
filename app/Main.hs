module Main where

import           Gematria
import           Hebrew

import           Transformation

main :: IO ()
main = do
  let word = prettyRead "שלום" :: HFWord
  putStrLn $ prettyShow word
  print word
  print $ computeGematria Hechrachi $ applyTransformation AkhasBeta word
