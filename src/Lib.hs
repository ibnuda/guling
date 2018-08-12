{-# LANGUAGE RecordWildCards #-}
module Lib
  ( someFunc
  ) where

import           Protolude

import           Conduit                 as C
import           Data.Conduit.Attoparsec
import           Data.Text               (pack)
import           Options.Applicative

import           OpsiJalan
import           Types
import           Urai

someFunc :: IO ()
someFunc = do
  Opsi {..} <- execParser infoopsi
  runConduitRes
    $  C.sourceFile opsiNamaBerkas
    .| conduitParser parseKomLog
    .| filterC (\(_, KomLog _ _ c _ _) -> c == opsiSeverity)
    .| takeC opsiJumlahMaksimal
    .| mapC posrangekomlogketeks
    .| iterMC putText
    .| sinkNull


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
