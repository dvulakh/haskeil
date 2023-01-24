module Search where

import           Gematria
import           Hebrew
import           Transformation

import           Control.Applicative
import           Data.Bifunctor

type Hop = (Gematria, Transformation)

allHops :: [Hop]
allHops = liftA2 (,) [minBound ..] [minBound ..]

applyHop :: Hop -> HFWord -> Int
applyHop = uncurry (.) . bimap computeGematria applyTransformation

oneHop :: HFWord -> HFWord -> [(Hop, Hop)]
oneHop w1 w2 = do
  h1 <- allHops
  let g1 = applyHop h1 w1
  h2 <- allHops
  let g2 = applyHop h2 w2
  [ (h1, h2) | g1 == g2 ]
