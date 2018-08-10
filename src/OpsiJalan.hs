module OpsiJalan where

import           Protolude           hiding (option)

import           Options.Applicative

import           Types

opsi :: Parser Opsi
opsi =
  Opsi
    <$> option
          auto
          (  long "jumlah-minimal"
          <> short 'i'
          <> metavar "JUMMIN"
          <> help "Jumlah opsi minimal"
          <> showDefault
          <> value 0
          )
    <*> option
          auto
          (  long "jumlah-maksimal"
          <> short 'a'
          <> metavar "JUMMAKS"
          <> help "Jumlah maksimal yang ditampilkan."
          <> showDefault
          <> value 10
          )
    <*> strOption
          (long "nama-berkas"
          <> short 'b'
          <> metavar "NAMABERKAS"
          <> help "Nama berkas log."
          )

eksekopsi :: Opsi -> FilePath
eksekopsi (Opsi minimal maksimal namaberkas)
  | minimal > maksimal = panic "tidak mungkin bisa"
  | otherwise          = namaberkas

infoopsi :: ParserInfo Opsi
infoopsi =
  info opsi (fullDesc <> progDesc "guling guling log4j" <> header "something.")
