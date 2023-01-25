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

toHFLetter :: Int -> HFLetter
toHFLetter = toEnum . (+ (-1))

fromHLetter :: HLetter -> Int
fromHLetter = (+1) . fromEnum

maxLetter :: Int
maxLetter = fromEnum (maxBound :: HLetter) + 1

applyTransformation :: Transformation -> HFWord -> HFWord
applyTransformation Aatat = id
applyTransformation Atbash =
  map $ toHFLetter . (maxLetter -) . fromHLetter . fromFinal
applyTransformation Albam =
  map
    $ toHFLetter
    . (`mod` maxLetter)
    . (+ (maxLetter `div` 2))
    . fromHLetter
    . fromFinal
applyTransformation Achbi = map $ \h ->
  let (group, position) = (fromEnum h +1) `divMod` halfLetter
  in  toHFLetter $ halfLetter * group + position
  where halfLetter = maxLetter `div` 2
applyTransformation AyakBakar = map $ toHFLetter . (`mod` 999) . (* 10) . fromHLetter . fromFinal
applyTransformation Ofanim    = map $ last . letterSpelling
applyTransformation AkhasBeta = map $ \h -> case fromFinal h of
  Tav -> FTav
  _ ->
    let (group, position) = (fromEnum h + 1) `divMod` 7
    in  toHFLetter (7 * group + position)
applyTransformation Avgad = map succ
