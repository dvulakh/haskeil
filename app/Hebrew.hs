{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia   #-}
{-# LANGUAGE InstanceSigs  #-}

module Hebrew where

import           Data.Bifunctor
import           Data.List
import           GHC.Generics                   ( Generic )
import           Test.QuickCheck.Arbitrary.Generic

class Pretty a where
  prettyShow :: a -> String
  prettyReadPrec :: String -> Maybe (a, String)
  prettyRead :: String -> a
  prettyRead s = case prettyReadPrec s of
                   Just (a, "") -> a
                   _            -> error $ "Failed to parse string " ++ s


data HLetter = Alef
             | Bet
             | Gimel
             | Dalet
             | Hey
             | Vav
             | Zayin
             | Chet
             | Tet
             | Yod
             | Chaf
             | Lamed
             | Mem
             | Nun
             | Samech
             | Ayin
             | Pay
             | Tzadi
             | Kuf
             | Reish
             | Shin
             | Tav
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving (Arbitrary)
    via GenericArbitrary HLetter


instance Pretty HLetter where
  prettyShow :: HLetter -> String
  prettyShow = prettyShow . toFinal
  prettyReadPrec :: String -> Maybe (HLetter, String)
  prettyReadPrec s = first fromFinal <$> prettyReadPrec s


data HFLetter = FAlef
              | FBet
              | FGimel
              | FDalet
              | FHey
              | FVav
              | FZayin
              | FChet
              | FTet
              | FYod
              | FChaf
              | FLamed
              | FMem
              | FNun
              | FSamech
              | FAyin
              | FPay
              | FTzadi
              | FKuf
              | FReish
              | FShin
              | FTav
              | FFChaf
              | FFMem
              | FFNun
              | FFPay
              | FFTzadi
  deriving (Bounded, Enum, Eq, Ord, Read, Show)


instance Pretty HFLetter where
  prettyShow :: HFLetter -> String
  prettyShow = pure . toChar
  prettyReadPrec :: String -> Maybe (HFLetter, String)
  prettyReadPrec s = do
    (a, s') <- uncons s
    l       <- ofChar a
    pure (l, s')


mapEnum :: (Enum a, Enum b) => (Int -> Int) -> a -> b
mapEnum = (. fromEnum) . (toEnum .)


toChar :: HFLetter -> Char
toChar FAlef   = 'א'
toChar FBet    = 'ב'
toChar FGimel  = 'ג'
toChar FDalet  = 'ד'
toChar FHey    = 'ה'
toChar FVav    = 'ו'
toChar FZayin  = 'ז'
toChar FChet   = 'ח'
toChar FTet    = 'ט'
toChar FYod    = 'י'
toChar FChaf   = 'כ'
toChar FLamed  = 'ל'
toChar FMem    = 'מ'
toChar FNun    = 'נ'
toChar FSamech = 'ס'
toChar FAyin   = 'ע'
toChar FPay    = 'פ'
toChar FTzadi  = 'צ'
toChar FKuf    = 'ק'
toChar FReish  = 'ר'
toChar FShin   = 'ש'
toChar FTav    = 'ת'
toChar FFChaf  = 'ך'
toChar FFMem   = 'ם'
toChar FFNun   = 'ן'
toChar FFPay   = 'ף'
toChar FFTzadi = 'ץ'


ofChar :: Char -> Maybe HFLetter
ofChar 'א' = Just FAlef
ofChar 'ב' = Just FBet
ofChar 'ג' = Just FGimel
ofChar 'ד' = Just FDalet
ofChar 'ה' = Just FHey
ofChar 'ו' = Just FVav
ofChar 'ז' = Just FZayin
ofChar 'ח' = Just FChet
ofChar 'ט' = Just FTet
ofChar 'י' = Just FYod
ofChar 'כ' = Just FChaf
ofChar 'ל' = Just FLamed
ofChar 'מ' = Just FMem
ofChar 'נ' = Just FNun
ofChar 'ס' = Just FSamech
ofChar 'ע' = Just FAyin
ofChar 'פ' = Just FPay
ofChar 'צ' = Just FTzadi
ofChar 'ק' = Just FKuf
ofChar 'ר' = Just FReish
ofChar 'ש' = Just FShin
ofChar 'ת' = Just FTav
ofChar 'ך' = Just FFChaf
ofChar 'ם' = Just FFMem
ofChar 'ן' = Just FFNun
ofChar 'ף' = Just FFPay
ofChar 'ץ' = Just FFTzadi
ofChar _   = Nothing


type HWord = [HLetter]
type HFWord = [HFLetter]


instance Pretty a => Pretty [a] where
  prettyShow :: [a] -> String
  prettyShow = (>>= prettyShow)
  prettyReadPrec :: String -> Maybe ([a], String)
  prettyReadPrec = Just . prettyReadPrec'
   where
    prettyReadPrec' :: Pretty a => String -> ([a], String)
    prettyReadPrec' s = maybe
      ([], s)
      (uncurry ($) . bimap (first . (:)) prettyReadPrec')
      (prettyReadPrec s)


finalize :: HLetter -> HFLetter
finalize Chaf    = FFChaf
finalize Mem     = FFMem
finalize Nun     = FFNun
finalize Pay     = FFPay
finalize Tzadi   = FFTzadi
finalize fletter = toFinal fletter


toFinal :: HLetter -> HFLetter
toFinal = toEnum . fromEnum


fromFinal :: HFLetter -> HLetter
fromFinal FFChaf  = Chaf
fromFinal FFMem   = Mem
fromFinal FFNun   = Nun
fromFinal FFPay   = Pay
fromFinal FFTzadi = Tzadi
fromFinal fletter = toEnum $ fromEnum fletter


removeFinals :: HFLetter -> HFLetter
removeFinals = toFinal . fromFinal


numberSpelling :: Int -> HFWord
numberSpelling 1 = prettyRead "אחד"
numberSpelling 2 = prettyRead "שתים"
numberSpelling 3 = prettyRead "שלש"
numberSpelling 4 = prettyRead "ארבע"
numberSpelling 5 = prettyRead "חמש"
numberSpelling 6 = prettyRead "שש"
numberSpelling 7 = prettyRead "שבע"
numberSpelling 8 = prettyRead "שמונה"
numberSpelling 9 = prettyRead "תשע"
numberSpelling 0 = prettyRead "אפס"
numberSpelling n = numberSpelling (n `div` 10) ++ numberSpelling (n `mod` 10)


letterSpelling :: HFLetter -> HFWord
letterSpelling h = prettyRead $ case fromFinal h of
  Alef   -> "אלף"
  Bet    -> "בית"
  Gimel  -> "גימל"
  Dalet  -> "דלת"
  Hey    -> "הא"
  Vav    -> "ויו"
  Zayin  -> "זין"
  Chet   -> "חית"
  Tet    -> "טית"
  Yod    -> "יוד"
  Chaf   -> "כף"
  Lamed  -> "למד"
  Mem    -> "מים"
  Nun    -> "נון"
  Samech -> "סמך"
  Ayin   -> "עין"
  Pay    -> "פי"
  Tzadi  -> "צדי"
  Kuf    -> "קוף"
  Reish  -> "ריש"
  Shin   -> "שין"
  Tav    -> "תיו"


properWord :: HWord -> HFWord
properWord w = case reverse w of
  (h : hs) -> reverse $ finalize h : map toFinal hs
  _        -> []
