module Types where

import           Protolude

import           Data.Time

data Severity
  = Info
  | Warn
  | Error
  | Fatal
  deriving (Read, Show, Enum, Eq, Ord)

data KomLog = KomLog
  { komlogHari     :: Day
  , komlogJam      :: TimeOfDay
  , komlogSeverity :: Severity
  , komlogMethod   :: ByteString
  } deriving (Show)
