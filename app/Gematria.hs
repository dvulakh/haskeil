module Gematria (Gematria(..), computeGematria) where

import           Hebrew

data Gematria = Hechrachi
              | Gadol
              | Kattan
              | Siduri
              | Boneh
              | Kidmi
              | Prati
              | Merubah
              | Meshulash
              | Akhor
              | Mispari
              | BeMilui
              | Neelam
              | Misafi
              | Kolel

gadolValue :: HFLetter -> Int
gadolValue h = 10 ^ zeros * (digit + 1)
  where (zeros, digit) = fromEnum h `divMod` 9

hechrachiValue :: HFLetter -> Int
hechrachiValue = gadolValue . finalFromTo

stam :: (HFLetter -> Int) -> HFWord -> Int
stam = (sum .) . map

computeGematria :: Gematria -> HFWord -> Int
computeGematria Hechrachi = stam hechrachiValue
computeGematria Gadol     = stam gadolValue
computeGematria Kattan    = stam $ (+1) . (`mod` 9) . fromEnum . finalFromTo
computeGematria Siduri    = stam fromEnum
computeGematria Boneh     = undefined
computeGematria Kidmi     = computeGematria Hechrachi . (enumFromTo FAlef =<<)
computeGematria Prati     = stam $ (^ (2 :: Int)) . hechrachiValue
computeGematria Merubah   = (^ (2 :: Int)) . computeGematria Hechrachi
computeGematria Meshulash = stam $ (^ (3 :: Int)) . hechrachiValue
computeGematria Akhor =
  sum . map (uncurry (*)) . zip [1 ..] . map hechrachiValue
computeGematria Mispari = undefined
computeGematria BeMilui = computeGematria Hechrachi . (letterSpelling =<<)
computeGematria Neelam  = computeGematria Hechrachi . (spell =<<)
  where spell h = filter (/= h) $ letterSpelling h
computeGematria Misafi = undefined
computeGematria Kolel  = undefined
