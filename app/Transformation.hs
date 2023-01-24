module Transformation (Transformation(..), applyTransformation) where
import           Hebrew

-- TODO: Test these!!
data Transformation = Aatat
                    | Atbash
                    | Albam
                    | Achbi
                    | AyakBakar
                    | Ofanim
                    | AkhasBeta
                    | Avgad
  deriving (Bounded, Enum, Show)

toHLetter :: Int -> HFLetter
toHLetter = toEnum . (+ (-1))

maxLetter :: Int
maxLetter = fromEnum (maxBound :: HLetter) + 1

applyTransformation :: Transformation -> HFWord -> HFWord
applyTransformation Aatat = id
applyTransformation Atbash =
  map $ toHLetter . (maxLetter -) . fromEnum . fromFinal
applyTransformation Albam =
  map
    $ toHLetter
    . (`mod` maxLetter)
    . (+ (maxLetter `div` 2))
    . (+1) . fromEnum
    . fromFinal
applyTransformation Achbi = map $ \h ->
  let (group, position) = (fromEnum h + 1) `divMod` halfLetter
  in  toEnum $ halfLetter * group + position
  where halfLetter = maxLetter `div` 2
applyTransformation AyakBakar = map $ toHLetter . (`mod` 999) . (* 10) . (+1) . fromEnum
applyTransformation Ofanim    = map $ last . letterSpelling
applyTransformation AkhasBeta = map $ \h -> case fromFinal h of
  Tav -> FTav
  _ ->
    let (group, position) = (fromEnum h + 1) `divMod` 7
    in  toEnum (7 * group + position)
applyTransformation Avgad = map succ
