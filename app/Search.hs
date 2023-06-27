module Search where

import           Gematria
import           Hebrew
import           Transformation

import           Control.Applicative
import           Text.Printf

type Hop = (PostProcess, Gematria, Transformation)

allHops :: [Hop]
allHops = liftA3 (,,) [minBound ..] [minBound ..] [minBound ..]

applyHop :: Hop -> HFWord -> Int
applyHop (p, g, t) w =
  applyPostProcess p w (computeGematria g $ applyTransformation t w)

oneHop :: HFWord -> HFWord -> [(Hop, Hop)]
oneHop w1 w2 = do
  h1 <- allHops
  let g1 = applyHop h1 w1
  h2 <- allHops
  let g2 = applyHop h2 w2
  [ (h1, h2) | g1 == g2 ]

printHop :: Hop -> HFWord -> String
printHop h@(p, g, t) w = printf
  "The word %s%s has value %d in Mispar %s%s."
  (prettyShow w)
  (if t == Aatat
    then ""
    else
      " turns into "
      ++ prettyShow (applyTransformation t w)
      ++ " under transformation "
      ++ linkToTransformation t
      ++ ". This"
  )
  (applyHop h w)
  (linkToGematria g)
  (explainPostProcess p)

wordToWord :: HFWord -> HFWord -> [String]
wordToWord w1 w2 =
  map (\(h1, h2) -> printHop h1 w1 ++ "\n" ++ printHop h2 w2) $ oneHop w1 w2

