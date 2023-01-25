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
fromHLetter = (+ 1) . fromEnum

maxLetter :: Int
maxLetter = fromEnum (maxBound :: HLetter) + 1

applyTransformation :: Transformation -> HFWord -> HFWord
applyTransformation Aatat = id
applyTransformation Atbash =
  map $ toEnum . (maxLetter -) . fromHLetter . fromFinal
applyTransformation Albam =
  map
    $ toHFLetter
    . (`mod` maxLetter)
    . (+ (maxLetter `div` 2))
    . fromHLetter
    . fromFinal
applyTransformation Achbi = map $ \h ->
  let (group, position) = fromEnum (removeFinals h) `divMod` halfLetter
  in  toHFLetter $ halfLetter * group + (halfLetter - position)
  where halfLetter = maxLetter `div` 2
applyTransformation AyakBakar =
  map $ toHFLetter . (`mod` 27) . (+10) . fromEnum
applyTransformation Ofanim    = map $ removeFinals . last . letterSpelling
applyTransformation AkhasBeta = map $ \h -> case fromFinal h of
  Tav -> FTav
  _ ->
    let (group, position) = fromEnum (fromFinal h) `divMod` 7
    in  toEnum (7 * ((group + 1) `mod` 3) + position)
applyTransformation Avgad = map $ toFinal . (\h -> case h of
                                                Tav -> Alef
                                                _ -> succ h) . fromFinal
