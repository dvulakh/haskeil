module Browser where

import           Hebrew
import           Search

import           Foreign.C.String

-- sets the output HTML element
foreign import javascript "((arr,offset) => document.getElementById('output').innerHTML = h$decodeUtf8z(arr,offset))"
  setOutput :: CString -> IO ()

-- read the contents of one of the HTML input box elements
foreign import javascript "((arr,offset) => h$encodeUtf8(document.getElementById(h$decodeUtf8z(arr,offset)).value))"
  getInputValueCS :: CString -> IO CString

getInputValue :: String -> IO CString
getInputValue = (`withCString` getInputValueCS)

cstrToCstr :: CString -> CString -> IO String
cstrToCstr s1 s2 = do
  w1 <- prettyRead <$> peekCString s1
  w2 <- prettyRead <$> peekCString s2
  pure $ head $ wordToWord w1 w2
