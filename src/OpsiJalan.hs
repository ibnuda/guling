module OpsiJalan where

import           Protolude           hiding (option)

import           Options.Applicative

import           Types

sever :: Parser Severity
sever = strOption (long "sever")

opsi :: Parser Opsi
opsi =
  Opsi
    <$> option
          auto
          (  long "jumlah-maksimal"
          <> short 'x'
          <> metavar "JUMMAKS"
          <> help "Jumlah maksimal yang ditampilkan."
          <> showDefault
          <> value 1
          )
    <*> strOption
          (  long "nama-berkas"
          <> short 'b'
          <> metavar "NAMABERKAS"
          <> help "Nama berkas log."
          )
    <*> option
          auto
          (  long "severity"
          <> short 's'
          <> metavar "SEVER"
          <> help "Tingkat kacau. fatal, error, warn, info. default error."
          <> value "error"
          )

eksekopsi :: Opsi -> FilePath
eksekopsi (Opsi maksimal namaberkas _)
  | 0 < maksimal = panic "tidak mungkin bisa"
  | otherwise    = namaberkas

infoopsi :: ParserInfo Opsi
infoopsi = info
  opsi
  (fullDesc <> progDesc "guling guling log4j" <> header "baca error wincil")
