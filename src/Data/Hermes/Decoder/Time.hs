-- | ISO 8601 Compatibility

module Data.Hermes.Decoder.Time
  ( day
  , localTime
  , month
  , quarter
  , timeOfDay
  , timeZone
  , utcTime
  , zonedTime
  ) where

import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.Time as ATime
import           Data.Text (Text)
import qualified Data.Time as Time
import qualified Data.Time.Calendar.Month.Compat as Time
import qualified Data.Time.Calendar.Quarter.Compat as Time
import qualified Data.Time.LocalTime as Local

import           Data.Hermes.Decoder.Types (Decoder)
import           Data.Hermes.Decoder.Value (withText)
import           Data.Hermes.SIMDJSON

-- | Run an attoparsec text parser as a hermes decoder.
runAttoDate :: AT.Parser a -> Text -> Decoder a
runAttoDate p t =
  case AT.parseOnly (p <* AT.endOfInput) t of
    Left err -> fail $ "Could not parse date: " <> err
    Right r  -> pure r
{-# INLINE runAttoDate #-}

-- | Parse a date of the form @[+,-]YYYY-MM-DD@.
day :: Value -> Decoder Time.Day
day = withText $ runAttoDate ATime.day

-- | Parse a date of the form @[+,-]YYYY-MM@.
month :: Value -> Decoder Time.Month
month = withText $ runAttoDate ATime.month

-- | Parse a date of the form @[+,-]YYYY-QN@.
quarter :: Value -> Decoder Time.Quarter
quarter = withText $ runAttoDate ATime.quarter

-- | Parse a time of the form @HH:MM[:SS[.SSS]]@.
timeOfDay :: Value -> Decoder Local.TimeOfDay
timeOfDay = withText $ runAttoDate ATime.timeOfDay

-- | Parse a time zone, and return 'Nothing' if the offset from UTC is
-- zero. (This makes some speedups possible.)
timeZone :: Value -> Decoder (Maybe Local.TimeZone)
timeZone = withText $ runAttoDate ATime.timeZone

-- | Parse a date and time, of the form @YYYY-MM-DD HH:MM[:SS[.SSS]]@.
-- The space may be replaced with a @T@. The number of seconds is optional
-- and may be followed by a fractional component.
localTime :: Value -> Decoder Local.LocalTime
localTime = withText $ runAttoDate ATime.localTime

-- | Behaves as 'zonedTime', but converts any time zone offset into a UTC time.
utcTime :: Value -> Decoder Time.UTCTime
utcTime = withText $ runAttoDate ATime.utcTime

-- | Parse a date with time zone info. Acceptable formats:
--
-- @YYYY-MM-DD HH:MM Z@
-- @YYYY-MM-DD HH:MM:SS Z@
-- @YYYY-MM-DD HH:MM:SS.SSS Z@
--
-- The first space may instead be a @T@, and the second space is
-- optional.  The @Z@ represents UTC.  The @Z@ may be replaced with a
-- time zone offset of the form @+0000@ or @-08:00@, where the first
-- two digits are hours, the @:@ is optional and the second two digits
-- (also optional) are minutes.
zonedTime :: Value -> Decoder Local.ZonedTime
zonedTime = withText $ runAttoDate ATime.zonedTime
