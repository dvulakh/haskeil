module Search where

import           Gematria
import           Hebrew
import           Transformation

import           Control.Applicative

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

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
  [ (h1, h2) | g1 == g2 && fst3 h2 == Shaveh ]

printHop :: Hop -> HFWord -> String
printHop h w =
  "The word "
    ++ prettyShow w
    ++ (if thd3 h == Aatat
         then ""
         else
           " turns into "
           ++ prettyShow (applyTransformation (thd3 h) w)
           ++ " under transformation "
           ++ show (thd3 h)
           ++ ". This"
       )
    ++ " has value "
    ++ show (applyHop h w)
    ++ " in Mispar "
    ++ show (snd3 h)
    ++ if fst3 h == Shaveh
         then "."
         else " after " ++ explainPostProcess (fst3 h) ++ "."

wordToWord :: HFWord -> HFWord -> [String]
wordToWord w1 w2 =
  map (\(h1, h2) -> printHop h1 w1 ++ "\n" ++ printHop h2 w2) $ oneHop w1 w2
