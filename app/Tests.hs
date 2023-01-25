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
import           Transformation

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
    [ testCase "Gematria hechrachi of חי"  test1
    , testCase "Gematria gadol of השם"     test2
    , testCase "Gematria kattan of לבן"    test3
    , testCase "Gematria siduri of בגד"    test4
    , testCase "Gematria kidmi of בגד"     test5
    , testCase "Gematria prati of בגד"     test6
    , testCase "Gematria merubah of בגד"   test7
    , testCase "Gematria meshulash of בגד" test8
    , testCase "Gematria akhor of בגד"     test9
    , testCase "Gematria bemilui of בגד"   test10
    , testCase "Gematria neelam of בגד"    test11
    ]
  , testGroup
    "Applying transformations"
    [ testCase "Transformation atbash on בראשית"     test12
    , testCase "Transformation achbi on בראשית"      test13
    , testCase "Transformation avgad on בראשית"      test14
    , testCase "Transformation albam on בראשית"      test15
    , testCase "Transformation ofanim on בראשית"     test16
    , testCase "Transformation akhas beta on בראשית" test17
    , testCase "Transformation atbash on דברים"      test18
    , testCase "Transformation achbi on דברים"       test19
    , testCase "Transformation avgad on דברים"       test20
    , testCase "Transformation albam on דברים"       test21
    , testCase "Transformation ofanim on דברים"      test22
    , testCase "Transformation akhas beta on דברים"  test23
    , testCase "Transformation ayak bakar on בראשית" test24
    , testCase "Transformation ayak bakar on דברים"  test25
    , testCase "Transformation atbash on all"        test26
    , testCase "Transformation achbi on all"         test27
    , testCase "Transformation avgad on all"         test28
    , testCase "Transformation albam on all"         test29
    , testCase "Transformation ofanim on all"        test30
    , testCase "Transformation akhas beta on all"    test31
    , testCase "Transformation ayak bakar on all"    test32
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

test7 :: IO ()
test7 = computeGematria Merubah (prettyRead "בגד") @?= 81

test8 :: IO ()
test8 = computeGematria Meshulash (prettyRead "בגד") @?= 99

test9 :: IO ()
test9 = computeGematria Akhor (prettyRead "בגד") @?= 20

test10 :: IO ()
test10 =
  computeGematria BeMilui (prettyRead "בגד")
    @?= 2
    +   10
    +   400
    +   3
    +   10
    +   40
    +   30
    +   4
    +   30
    +   400

test11 :: IO ()
test11 =
  computeGematria Neelam (prettyRead "בגד")
    @?= 10
    +   400
    +   10
    +   40
    +   30
    +   30
    +   400

test12 :: IO ()
test12 =
  applyTransformation Atbash (prettyRead "בראשית") @?= prettyRead "שגתבמא"

test13 :: IO ()
test13 =
  applyTransformation Achbi (prettyRead "בראשית") @?= prettyRead "ינכמבל"

test14 :: IO ()
test14 =
  applyTransformation Avgad (prettyRead "בראשית") @?= prettyRead "גשבתכא"

test15 :: IO ()
test15 =
  applyTransformation Albam (prettyRead "בראשית") @?= prettyRead "מטלישכ"

test16 :: IO ()
test16 =
  applyTransformation Ofanim (prettyRead "בראשית") @?= prettyRead "תשפנדו"

test17 :: IO ()
test17 =
  applyTransformation AkhasBeta (prettyRead "בראשית") @?= prettyRead "טוחזפת"

test18 :: IO ()
test18 = applyTransformation Atbash (prettyRead "דברים") @?= prettyRead "קשגמי"

test19 :: IO ()
test19 = applyTransformation Achbi (prettyRead "דברים") @?= prettyRead "חינבש"

test20 :: IO ()
test20 = applyTransformation Avgad (prettyRead "דברים") @?= prettyRead "הגשכנ"

test21 :: IO ()
test21 = applyTransformation Albam (prettyRead "דברים") @?= prettyRead "סמטשב"

test22 :: IO ()
test22 = applyTransformation Ofanim (prettyRead "דברים") @?= prettyRead "תתשדמ"

test23 :: IO ()
test23 =
  applyTransformation AkhasBeta (prettyRead "דברים") @?= prettyRead "כטופר"

test24 :: IO ()
test24 =
  applyTransformation AyakBakar (prettyRead "בראשית") @?= prettyRead "כביגקד"

test25 :: IO ()
test25 =
  applyTransformation AyakBakar (prettyRead "דברים") @?= prettyRead "מכבקו"

test26 :: IO ()
test26 = applyTransformation Atbash (prettyRead "צ") @?= prettyRead "ה"

test27 :: IO ()
test27 = applyTransformation Achbi (prettyRead "צ") @?= prettyRead "ע"

test28 :: IO ()
test28 = applyTransformation Avgad (prettyRead "צ") @?= prettyRead "ק"

test29 :: IO ()
test29 = applyTransformation Albam (prettyRead "צ") @?= prettyRead "ז"

test30 :: IO ()
test30 = applyTransformation Ofanim (prettyRead "צ") @?= prettyRead "י"

test31 :: IO ()
test31 = applyTransformation AkhasBeta (prettyRead "צ") @?= prettyRead "ד"

test32 :: IO ()
test32 = applyTransformation AyakBakar (prettyRead "צ") @?= prettyRead "ץ"

