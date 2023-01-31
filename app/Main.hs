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
  putStrLn $ head $ wordToWord (prettyRead "משה") (prettyRead "השטן")
  putStrLn $ head $ wordToWord (prettyRead "כבודבשבילנשים")
                               (prettyRead "ישיבתאםאייטי")
  mapM_ putStrLn $ wordToWord (prettyRead "דוד") (prettyRead "פדיוןהבן")
