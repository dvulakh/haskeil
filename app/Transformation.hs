module Transformation (Transformation (..), applyTransformation) where
import           Hebrew

data Transformation = Atbash
                    | Albam
                    | Achbi
                    | AyakBakar
                    | Ofanim
                    | AkhasBeta
                    | Avgad

maxLetter :: Int
maxLetter = fromEnum (maxBound :: HLetter)

applyTransformation :: Transformation -> HFWord -> HFWord
applyTransformation Atbash  = map $ toEnum . ((maxLetter-1) -) . fromEnum . fromFinal
applyTransformation Albam   = map $ toEnum . (`mod` maxLetter) . (+(maxLetter `div` 2)) . fromEnum . fromFinal
applyTransformation Achbi = map $ \h->let (group, position) = fromEnum h `divMod` halfLetter in toEnum $ halfLetter * group + position
                where halfLetter = maxLetter `div` 2
applyTransformation AyakBakar = map $ toEnum . (`mod` 999) . (*10) . fromEnum
applyTransformation Ofanim = map $ last . letterSpelling
applyTransformation AkhasBeta = map $ \h -> case fromFinal h of
        Tav -> FTav
        _ -> let (group, position) = fromEnum h `divMod` 7 in toEnum (7 * group + position)
applyTransformation Avgad = map succ