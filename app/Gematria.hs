module Gematria
  ( Gematria(..)
  , PostProcess(..)
  , computeGematria
  , applyPostProcess
  , explainPostProcess
  , gadolValue
  , hechrachiValue
  ) where

import           Hebrew

data Gematria = Hechrachi
              | Gadol
              | Kattan
              | Siduri
              -- | Boneh
              | Kidmi
              | Prati
              | Merubah
              | Meshulash
              | Akhor
              -- | Mispari
              | BeMilui
              | Neelam
              -- | Misafi
              -- | Kolel
  deriving (Bounded, Enum, Show)

data PostProcess = Shaveh
                 | Misafi
                 | Kolel
  deriving (Bounded, Enum, Eq, Show)

explainPostProcess :: PostProcess -> String
explainPostProcess Shaveh = ""
explainPostProcess Misafi = " after adding the number of letters in the word"
explainPostProcess Kolel  = " after adding one for Hashem"

gadolValue :: HFLetter -> Int
gadolValue h = 10 ^ zeros * (digit + 1)
  where (zeros, digit) = fromEnum h `divMod` 9

hechrachiValue :: HFLetter -> Int
hechrachiValue = gadolValue . removeFinals

stam :: (HFLetter -> Int) -> HFWord -> Int
stam = (sum .) . map

computeGematria :: Gematria -> HFWord -> Int
computeGematria Hechrachi = stam hechrachiValue
computeGematria Gadol     = stam gadolValue
computeGematria Kattan    = stam $ (+ 1) . (`mod` 9) . fromEnum . removeFinals
computeGematria Siduri    = stam $ (+ 1) . fromEnum
--computeGematria Boneh     = undefined
computeGematria Kidmi     = computeGematria Hechrachi . (enumFromTo FAlef =<<)
computeGematria Prati     = stam $ (^ (2 :: Int)) . hechrachiValue
computeGematria Merubah   = (^ (2 :: Int)) . computeGematria Hechrachi
computeGematria Meshulash = stam $ (^ (3 :: Int)) . hechrachiValue
computeGematria Akhor     = sum . zipWith (*) [1 ..] . map hechrachiValue
--computeGematria Mispari   = undefined
computeGematria BeMilui   = computeGematria Hechrachi . (letterSpelling =<<)
computeGematria Neelam    = computeGematria Hechrachi . (spell =<<)
  where spell h = filter (/= h) $ letterSpelling h

applyPostProcess :: PostProcess -> HFWord -> Int -> Int
applyPostProcess Shaveh _ = id
applyPostProcess Misafi h = (+ length h)
applyPostProcess Kolel  _ = (+ 1)
