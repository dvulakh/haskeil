module Search where

import Hebrew
import Transformation
import Gematria

import Data.Bifunctor
import Control.Applicative
import Data.Maybe

type Hop = (Gematria, Transformation)

allHops :: [Hop]
allHops = liftA2 (,) [minBound ..] [minBound ..]

applyHop :: Hop -> HFWord -> Int
applyHop = uncurry (.) . bimap computeGematria applyTransformation

oneHop :: HFWord -> HFWord -> [(Hop,Hop)]
oneHop w1 w2 = let gem1 = map (\h -> (h, applyHop h w1)) allHops in let gem2 = map (\h -> (h, applyHop h w2)) allHops
                    in catMaybes $ liftA2 (\(h1,g1) (h2,g2) -> if g1==g2 then Just (h1,h2) else Nothing) gem1 gem2