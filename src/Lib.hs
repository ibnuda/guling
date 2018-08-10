{-# LANGUAGE RecordWildCards #-}
module Lib
  ( someFunc
  ) where

import           Protolude

import           Conduit                 as C
import           Data.Conduit.Attoparsec
import           Data.Text               (pack)

import           Types
import           Urai

someFunc :: IO ()
someFunc = do
  forM_ [0 .. 6 :: Int] $ \_ -> putText "doing something very stupid."
  something <- do
    runConduitRes
       $ C.sourceFile "gedhe.log"
      .| conduitParser parseKomLog
      .| filterC (\(_, KomLog _ _ c _ _) -> c == Error)
      .| mapC posrangekomlogketeks
      .| iterMC putText
      .| lengthC
  print (something :: Int)

posrangekomlogketeks :: (PositionRange, KomLog) -> Text
posrangekomlogketeks (PositionRange {..}, KomLog {..}) =
  let Position {..} = posRangeStart
      keteks        = pack . show
  in  "baris: "
      <> keteks posLine
      <> ", kolom: "
      <> keteks posCol
      <> ", pesan: "
      <> komlogErrorMessage
