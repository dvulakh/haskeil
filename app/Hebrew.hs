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
toChar FAlef   = '??'
toChar FBet    = '??'
toChar FGimel  = '??'
toChar FDalet  = '??'
toChar FHey    = '??'
toChar FVav    = '??'
toChar FZayin  = '??'
toChar FChet   = '??'
toChar FTet    = '??'
toChar FYod    = '??'
toChar FChaf   = '??'
toChar FLamed  = '??'
toChar FMem    = '??'
toChar FNun    = '??'
toChar FSamech = '??'
toChar FAyin   = '??'
toChar FPay    = '??'
toChar FTzadi  = '??'
toChar FKuf    = '??'
toChar FReish  = '??'
toChar FShin   = '??'
toChar FTav    = '??'
toChar FFChaf  = '??'
toChar FFMem   = '??'
toChar FFNun   = '??'
toChar FFPay   = '??'
toChar FFTzadi = '??'


ofChar :: Char -> Maybe HFLetter
ofChar '??' = Just FAlef
ofChar '??' = Just FBet
ofChar '??' = Just FGimel
ofChar '??' = Just FDalet
ofChar '??' = Just FHey
ofChar '??' = Just FVav
ofChar '??' = Just FZayin
ofChar '??' = Just FChet
ofChar '??' = Just FTet
ofChar '??' = Just FYod
ofChar '??' = Just FChaf
ofChar '??' = Just FLamed
ofChar '??' = Just FMem
ofChar '??' = Just FNun
ofChar '??' = Just FSamech
ofChar '??' = Just FAyin
ofChar '??' = Just FPay
ofChar '??' = Just FTzadi
ofChar '??' = Just FKuf
ofChar '??' = Just FReish
ofChar '??' = Just FShin
ofChar '??' = Just FTav
ofChar '??' = Just FFChaf
ofChar '??' = Just FFMem
ofChar '??' = Just FFNun
ofChar '??' = Just FFPay
ofChar '??' = Just FFTzadi
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
numberSpelling 1 = prettyRead "??????"
numberSpelling 2 = prettyRead "????????"
numberSpelling 3 = prettyRead "??????"
numberSpelling 4 = prettyRead "????????"
numberSpelling 5 = prettyRead "??????"
numberSpelling 6 = prettyRead "????"
numberSpelling 7 = prettyRead "??????"
numberSpelling 8 = prettyRead "??????????"
numberSpelling 9 = prettyRead "??????"
numberSpelling 0 = prettyRead "??????"
numberSpelling n = numberSpelling (n `div` 10) ++ numberSpelling (n `mod` 10)


letterSpelling :: HFLetter -> HFWord
letterSpelling h = prettyRead $ case fromFinal h of
  Alef   -> "??????"
  Bet    -> "??????"
  Gimel  -> "????????"
  Dalet  -> "??????"
  Hey    -> "????"
  Vav    -> "??????"
  Zayin  -> "??????"
  Chet   -> "??????"
  Tet    -> "??????"
  Yod    -> "??????"
  Chaf   -> "????"
  Lamed  -> "??????"
  Mem    -> "??????"
  Nun    -> "??????"
  Samech -> "??????"
  Ayin   -> "??????"
  Pay    -> "????"
  Tzadi  -> "??????"
  Kuf    -> "??????"
  Reish  -> "??????"
  Shin   -> "??????"
  Tav    -> "??????"


properWord :: HWord -> HFWord
properWord w = case reverse w of
  (h : hs) -> reverse $ finalize h : map toFinal hs
  _        -> []
