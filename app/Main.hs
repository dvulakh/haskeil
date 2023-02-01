module Main where

--import           Gematria
--import           Hebrew
--import           Search
--import           Transformation

import System.IO  
import Hebrew (Pretty (..))
import Search (wordToWord)
import System.Random
--import Control.Monad
{-
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
-}

main :: IO ()
main = do
        --let list = []
        handle <- openFile "words/words.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
        let num = fst $ uniformR (1 :: Int, length singlewords :: Int) pureGen
              where pureGen = mkStdGen 138
        let list = generateWordList (singlewords!!num) singlewords
        putStrLn $ singlewords!!num
        print list
        hClose handle

checkWords :: String -> String -> Bool
checkWords w1 w2 = case wordToWord (prettyRead w1) (prettyRead w2) of
                      [] -> True
                      _  -> False

generateWordList :: String -> [String] -> [String]
generateWordList cw = filter (checkWords cw)