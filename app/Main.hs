module Main where

import           Gematria
import           Hebrew
import           Search
import           Transformation

main :: IO ()
main = do
  let word = prettyRead "שלום" :: HFWord
  putStrLn $ prettyShow word
  print word
  print $ computeGematria Hechrachi $ applyTransformation AkhasBeta word
  print $ head $ oneHop (prettyRead "תריג") (prettyRead "גרתי")
  print $ oneHop (prettyRead "משה") (prettyRead "השטן")
  print $ oneHop (prettyRead "כבודבשבילנשים") (prettyRead "ישיבתאםאייטיי")
