module Main where

import           Test.Framework                 ( Test
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Framework.Providers.HUnit ( testCase )
import           Test.Framework.Providers.QuickCheck2
                                                ( testProperty )
import           Test.HUnit                     ( (@?=) )

import           Gematria
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
  , testGroup "Calculating Gematriyos" [testCase "Gematria of חי" test1]
  ]

ofFinalToFinal :: HLetter -> Bool
ofFinalToFinal l = fromFinal (toFinal l) == l

readShow :: HLetter -> Bool
readShow l = prettyRead (prettyShow l) == l

test1 :: IO ()
test1 = computeGematria Hechrachi (prettyRead "חי") @?= 18
