module Urai where

import           Protolude                        hiding (option, takeWhile, take)

import           Data.Attoparsec.ByteString.Char8 as BC
import           Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8            as BS
import           Data.Fixed
import           Data.Time
import           Unsafe.Coerce                    (unsafeCoerce)

import           Types

parseSeverity :: Parser Severity
parseSeverity =
  (string "INFO" >> return Info)
    <|> (string "WARN" >> return Warn)
    <|> (string "ERROR" >> return Error)
    <|> (string "FATAL" >> return Fatal)

parseMethod :: Parser ByteString
parseMethod = do
  _         <- char '['
  something <- takeWhile (/= ']')
  return something

parseTanggal :: Parser Day
parseTanggal = do
  absOrNeg <- negate <$ char '-' <|> identity <$ char '+' <|> return identity
  y        <- decimal <* char '-'
  m        <- duaDigit <* char '-'
  d        <- duaDigit
--maybe (panic "Tanggal ra mutu.") return (fromGregorianValid (absOrNeg y) m d)
  return $ fromGregorian (absOrNeg y) m d

parseJam :: Parser TimeOfDay
parseJam = do
  h <- duaDigit
  m <- char ':' *> duaDigit
  s <- option 0 (char ':' *> parseDetik)
  if h < 24 && m < 60 && s < 61
    then return (TimeOfDay h m s)
    else panic "Jam ra mutu."

-- From attoparsec-iso8601
parseDetik :: Parser Pico
parseDetik = do
  detik <- duaDigit
  mc    <- peekChar
  case mc of
    Just ',' -> do
      t <- anyChar *> takeWhile1 isDigit
      return $! parsePicos detik t
    _ -> return $! fromIntegral detik
 where
  parsePicos a0 t = unsafeCoerce (fromIntegral (t' * 10 ^ n) :: Integer)
   where
    T n t' = foldl' step (T 12 (fromIntegral a0)) $ BS.unpack t
    step ma@(T m a) c
      | m <= 0    = ma
      | otherwise = T (m - 1) (10 * a + fromIntegral (ord c) .&. 15)

data T = T {-# UNPACK #-} !Int {-# UNPACK #-} !Int64

tanggalDiDepan :: Parser ()
tanggalDiDepan =
  lookAhead (parseTanggal *> parseSeverity *> parseMethod) *> return ()

duaDigit :: Parser Int
duaDigit = do
  a <- digit
  b <- digit
  let c2d c = ord c .&. 15
  return $! c2d a * 10 + c2d b


-- | Ada yang salah disini.
--   Mbuh. Ngantuk.
parseKomLog :: Parser KomLog
parseKomLog = do
  tanggal  <- parseTanggal
  _        <- space
  jam      <- parseJam
  _        <- space
  severity <- parseSeverity
  _        <- skipSpace
  method   <- parseMethod
  _        <- manyTill anyChar $ endOfInput <|> tanggalDiDepan
  return $ KomLog tanggal jam severity method
