module Main where

import           Test.Framework                 ( Test
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Framework.Providers.QuickCheck2
                                                ( testProperty )

import           Hebrew

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup
      "Manipulating Hebrew Letters"
      [ testProperty "ofFinal . toFinal ≡ id"       ofFinalToFinal
      , testProperty "prettyRead . prettyShow ≡ id" readShow
      ]
  ]

ofFinalToFinal :: HLetter -> Bool
ofFinalToFinal l = ofFinal (toFinal l) == l

readShow :: HLetter -> Bool
readShow l = prettyRead (prettyShow l) == l
