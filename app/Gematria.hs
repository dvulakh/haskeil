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
  where (zeros, digit) = (fromEnum h - 1) `divMod` 9

hechrachiValue :: HFLetter -> Int
hechrachiValue = gadolValue . toFinal . fromFinal

computeGematria :: Gematria -> HFWord -> Int
computeGematria Hechrachi = sum . map hechrachiValue
computeGematria Gadol     = sum . map gadolValue
computeGematria Kattan    = sum . map ((`mod` 9) . (+ (-1)) . fromEnum)
computeGematria Siduri    = sum . map fromEnum
computeGematria Boneh     = undefined
computeGematria Kidmi = sum . map (\h -> sum $ map hechrachiValue [FAlef .. h])
computeGematria Prati     = sum . map ((^ 2) . hechrachiValue)
computeGematria Merubah   = (^ 2) . computeGematria Hechrachi
computeGematria Meshulash = sum . map ((^ 3) . hechrachiValue)
computeGematria Akhor =
  sum . map (uncurry (*)) . zip [1 ..] . map hechrachiValue
computeGematria Mispari = undefined
computeGematria BeMilui = computeGematria Hechrachi . (letterSpelling =<<)
computeGematria Neelam  = computeGematria Hechrachi . (spell =<<)
  where spell h = filter (/= h) $ letterSpelling h
computeGematria Misafi = undefined
computeGematria Kolel  = undefined
