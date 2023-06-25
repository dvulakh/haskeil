module Main where

import           Browser
import           Gematria
import           Hebrew
import           Search
import           Transformation

import           Foreign.C.String

main :: IO ()
main = do
  from <- getInputValue "from_box"
  to   <- getInputValue "to_box"
  out  <- do
    fs <- peekCString from
    ts <- peekCString to
    if fs == "" || ts == ""
      then pure "Press 'Go' when ready"
      else cstrToCstr from to
  withCString out setOutput

