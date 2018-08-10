module Lib
  ( someFunc
  ) where

import           Protolude

import           Conduit                 as C
import           Data.Conduit.Attoparsec
import           Data.Text               (pack)

import           Urai

someFunc :: IO ()
someFunc = do
  forM_ [0 .. 6 :: Int] $ \_ -> putText "doing something very stupid."
  something <- do
    runConduitRes
       $ C.sourceFile "cilik.log"
      .| conduitParser parseKomLog
      .| iterMC (putText . pack . show)
      .| lengthC
  print (something :: Int)
