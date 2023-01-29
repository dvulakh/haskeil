module Transformation (Transformation(..), applyTransformation) where
import           Hebrew

data Transformation = Aatat
                    | Atbash
                    | Albam
                    | Achbi
                    | AyakBakar
                    | Ofanim
                    | AkhasBeta
                    | Avgad
  deriving (Eq, Bounded, Enum, Show)

maxLetter :: Int
maxLetter = fromEnum (maxBound :: HLetter) + 1

halfLetter :: Int
halfLetter = maxLetter `div` 2

applyTransformation :: Transformation -> HFWord -> HFWord
applyTransformation Aatat  = id
applyTransformation Atbash = map $ mapEnum (maxLetter - 1 -) . fromFinal
applyTransformation Albam =
  map $ mapEnum ((`mod` maxLetter) . (+ halfLetter)) . fromFinal
applyTransformation Achbi = map $ \h ->
  let (group, position) = fromEnum (removeFinals h) `divMod` halfLetter
  in  toEnum $ halfLetter * group + (halfLetter - position) - 1
applyTransformation AyakBakar =
  map $ mapEnum $ (`mod` (1 + fromEnum (maxBound :: HFLetter))) . (+ 9)
applyTransformation Ofanim    = map $ last . letterSpelling
applyTransformation AkhasBeta = map $ \h -> case fromFinal h of
  Tav -> FTav
  _ ->
    let (group, position) = fromEnum (fromFinal h) `divMod` 7
    in  toEnum (7 * ((group + 1) `mod` 3) + position)
applyTransformation Avgad =
  map
    $ toFinal
    . (\h -> case h of
        Tav -> Alef
        _   -> succ h
      )
    . fromFinal
