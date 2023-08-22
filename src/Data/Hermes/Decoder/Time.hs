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

import qualified Data.Time.FromText as Time
import           Data.Text (Text)
import qualified Data.Time as Time
import qualified Data.Time.Calendar.Month.Compat as Time
import qualified Data.Time.Calendar.Quarter.Compat as Time
import qualified Data.Time.LocalTime as Local

import           Data.Hermes.Decoder.Internal (Decoder(..))
import           Data.Hermes.Decoder.Value (withText)

-- | Run a Text parser as a Decoder.
runParser :: (Text -> Either String a) -> Text -> Decoder a
runParser p t =
  case p t of
    Left err -> fail $ "Could not parse date: " <> err
    Right r  -> pure r
{-# INLINE runParser #-}

-- | Parse a date of the form [+-]YYYY-MM-DD.
-- The year must contain at least 4 digits, to avoid the Y2K problem: a
-- two-digit year YY may mean YY, 19YY, or 20YY, and we make it an error to
-- prevent the ambiguity. Years from 0000 to 0999 must thus be zero-padded. The
-- year may have more than 4 digits.
day :: Decoder Time.Day
day = withText $ runParser Time.parseDay

-- | Parse a date of the form @[+,-]YYYY-MM@.
month :: Decoder Time.Month
month = withText $ runParser Time.parseMonth

-- | Parse a date of the form @[+,-]YYYY-QN@.
quarter :: Decoder Time.Quarter
quarter = withText $ runParser Time.parseQuarter

-- | Parse a time of the form @HH:MM[:SS[.SSS]]@.
timeOfDay :: Decoder Local.TimeOfDay
timeOfDay = withText $ runParser Time.parseTimeOfDay

-- | Parse a time zone, and return 'Nothing' if the offset from UTC is
-- zero. (This makes some speedups possible.)
timeZone :: Decoder Local.TimeZone
timeZone = withText $ runParser Time.parseTimeZone

-- | Parse a date and time, of the form @YYYY-MM-DD HH:MM[:SS[.SSS]]@.
-- The space may be replaced with a @T@. The number of seconds is optional
-- and may be followed by a fractional component.
localTime :: Decoder Local.LocalTime
localTime = withText $ runParser Time.parseLocalTime

-- | Behaves as 'zonedTime', but converts any time zone offset into a UTC time.
utcTime :: Decoder Time.UTCTime
utcTime = withText $ runParser Time.parseUTCTime

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
zonedTime :: Decoder Local.ZonedTime
zonedTime = withText $ runParser Time.parseZonedTime
