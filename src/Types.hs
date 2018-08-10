module Types where

import           Protolude

import           Data.Text as T
import           Data.Time

data Severity
  = Info
  | Warn
  | Error
  | Fatal
  deriving (Read, Show, Enum, Eq, Ord)

data KomLog = KomLog
  { komlogHari         :: Day
  , komlogJam          :: TimeOfDay
  , komlogSeverity     :: Severity
  , komlogMethod       :: ByteString
  , komlogErrorMessage :: Text
  } deriving (Show)

instance Eq KomLog where
  a == b =
    komlogSeverity a == komlogSeverity b && komlogMethod a == komlogMethod b

-- | Mana dulu yang paling kacau?
-- | Haruskah `Fatal` satu baris lebih penting
-- | daripada `Error` seratus baris?
-- | Gampang, sih. Dipikir nanti.
instance Ord KomLog where
  compare a b =
    compare (T.length $ komlogErrorMessage a) (T.length $ komlogErrorMessage b)


data Opsi = Opsi
  { opsiJumlahMinimal  :: Int
  , opsiJumlahMaksimal :: Int
  , opsiNamaBerkas     :: FilePath
  }
