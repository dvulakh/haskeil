import Hebrew (HWord, HFWord, HLetter (..), HFLetter (..), fromFinal)
data Transformation = Atbash
                    | Albam
                    | Achbi
                    | AyakBakar
                    | Ofanim
                    | AkhasBeta
                    | Avgad

applyTransformation :: Transformation -> HFWord -> HFWord
applyTransformation Atbash = undefined
applyTransformation Albam = undefined
applyTransformation _ = undefined