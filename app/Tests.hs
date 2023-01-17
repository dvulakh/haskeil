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
  , testGroup
    "Calculating Gematriyos"
    [ testCase "Gematria hechrachi of חי" test1
    , testCase "Gematria gadol of השם"    test2
    , testCase "Gematria kattan of לבן"   test3
    , testCase "Gematria siduri of בגד"   test4
    , testCase "Gematria kidmi of בגד"    test5
    , testCase "Gematria prati of בגד"    test6
    ]
  ]

ofFinalToFinal :: HLetter -> Bool
ofFinalToFinal l = fromFinal (toFinal l) == l

readShow :: HLetter -> Bool
readShow l = prettyRead (prettyShow l) == l

test1 :: IO ()
test1 = computeGematria Hechrachi (prettyRead "חי") @?= 18

test2 :: IO ()
test2 = computeGematria Gadol (prettyRead "השם") @?= 905

test3 :: IO ()
test3 = computeGematria Kattan (prettyRead "לבן") @?= 10

test4 :: IO ()
test4 = computeGematria Siduri (prettyRead "בגד") @?= 9

test5 :: IO ()
test5 = computeGematria Kidmi (prettyRead "בגד") @?= 19

test6 :: IO ()
test6 = computeGematria Prati (prettyRead "בגד") @?= 29
