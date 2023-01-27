module Search where

import           Gematria
import           Hebrew
import           Transformation

import           Control.Applicative

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
