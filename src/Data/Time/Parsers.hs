{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module:      Data.Time.Parsers (Data.Aeson.Parser.Time)
-- Copyright:   (c) 2015 Bryan O'Sullivan, 2015 Oleg Grenrus
-- License:     BSD3
-- Maintainer:  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Parsers for parsing dates and times.

module Data.Time.Parsers
    ( day
    , month
    , localTime
    , timeOfDay
    , timeZone
    , utcTime
    , zonedTime
    , DateParsing
    ) where

import Control.Applicative     (optional, some, (<|>))
import Control.Monad           (void, when)
import Data.Bits               ((.&.))
import Data.Char               (isDigit, ord)
import Data.Fixed              (Pico)
import Data.Int                (Int64)
import Data.List               (foldl')
import Data.Maybe              (fromMaybe)
import Data.Time.Calendar      (Day, fromGregorianValid)
import Data.Time.Clock         (UTCTime (..))
import Text.Parser.Char        (CharParsing (..), digit)
import Text.Parser.Combinators (unexpected)
import Text.Parser.LookAhead   (LookAheadParsing (..))
import Unsafe.Coerce           (unsafeCoerce)

import qualified Data.Time.LocalTime as Local

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((*>), (<$), (<$>), (<*), (<*>))
#endif

type DateParsing m = (CharParsing m, LookAheadParsing m, Monad m)

toPico :: Integer -> Pico
toPico = unsafeCoerce

-- | Parse a month of the form @YYYY-MM@
month :: DateParsing m => m (Integer, Int)
month = do
  s <- negate <$ char '-' <|> id <$ char '+' <|> return id
  y <- decimal
  _ <- char '-'
  m <- twoDigits
  if 1 <= m && m <= 12
      then return (s y, m)
      else unexpected "Invalid month"
{-# INLINE month #-}

-- | Parse a date of the form @YYYY-MM-DD@.
day :: DateParsing m => m Day
day = do
  s <- negate <$ char '-' <|> id <$ char '+' <|> return id
  y <- decimal
  _ <- char '-'
  m <- twoDigits
  _ <- char '-'
  d <- twoDigits
  maybe (unexpected "invalid date") return (fromGregorianValid (s y) m d)

-- | Parse a two-digit integer (e.g. day of month, hour).
twoDigits :: DateParsing m => m Int
twoDigits = do
  a <- digit
  b <- digit
  let c2d c = ord c .&. 15
  return $! c2d a * 10 + c2d b

-- | Parse a time of the form @HH:MM:SS[.SSS]@.
timeOfDay :: DateParsing m => m Local.TimeOfDay
timeOfDay = do
  h <- twoDigits <* char ':'
  m <- twoDigits <* char ':'
  s <- seconds
  if h < 24 && m < 60 && s < 61
    then return (Local.TimeOfDay h m s)
    else unexpected "invalid time"

data T = T {-# UNPACK #-} !Int {-# UNPACK #-} !Int64

-- | Parse a count of seconds, with the integer part being two digits
-- long.
seconds :: DateParsing m => m Pico
seconds = do
  real <- twoDigits
  mc <- peekChar
  case mc of
    Just '.' -> do
      t <- anyChar *> some digit
      return $! parsePicos real t
    _ -> return $! fromIntegral real
 where
  parsePicos a0 t = toPico (fromIntegral (t' * 10^n))
    where T n t'  = foldl' step (T 12 (fromIntegral a0)) t
          step ma@(T m a) c
              | m <= 0    = ma
              | otherwise = T (m-1) (10 * a + fromIntegral (ord c) .&. 15)

-- | Parse a time zone, and return 'Nothing' if the offset from UTC is
-- zero. (This makes some speedups possible.)
timeZone :: DateParsing m => m (Maybe Local.TimeZone)
timeZone = do
  let maybeSkip c = do ch <- peekChar'; when (ch == c) (void anyChar)
  maybeSkip ' '
  ch <- satisfy $ \c -> c == 'Z' || c == '+' || c == '-'
  if ch == 'Z'
    then return Nothing
    else do
      h <- twoDigits
      mm <- peekChar
      m <- case mm of
             Just ':'           -> anyChar *> twoDigits
             Just d | isDigit d -> twoDigits
             _                  -> return 0
      let off | ch == '-' = negate off0
              | otherwise = off0
          off0 = h * 60 + m
      case undefined of
        _   | off == 0 ->
              return Nothing
            | off < -720 || off > 840 || m > 59 ->
              unexpected "invalid time zone offset"
            | otherwise ->
              let !tz = Local.minutesToTimeZone off
              in return (Just tz)

-- | Parse a date and time, of the form @YYYY-MM-DD HH:MM:SS@.
-- The space may be replaced with a @T@.  The number of seconds may be
-- followed by a fractional component.
localTime :: DateParsing m => m Local.LocalTime
localTime = Local.LocalTime <$> day <* daySep <*> timeOfDay
  where daySep = satisfy (\c -> c == 'T' || c == ' ')

-- | Behaves as 'zonedTime', but converts any time zone offset into a
-- UTC time.
utcTime :: DateParsing m => m UTCTime
utcTime = f <$> localTime <*> timeZone
  where
    f :: Local.LocalTime -> Maybe Local.TimeZone -> UTCTime
    f (Local.LocalTime d t) Nothing =
        let !tt = Local.timeOfDayToTime t
        in UTCTime d tt
    f lt (Just tz) = Local.localTimeToUTC tz lt

-- | Parse a date with time zone info. Acceptable formats:
--
-- @YYYY-MM-DD HH:MM:SS Z@
--
-- The first space may instead be a @T@, and the second space is
-- optional.  The @Z@ represents UTC.  The @Z@ may be replaced with a
-- time zone offset of the form @+0000@ or @-08:00@, where the first
-- two digits are hours, the @:@ is optional and the second two digits
-- (also optional) are minutes.
zonedTime :: DateParsing m => m Local.ZonedTime
zonedTime = Local.ZonedTime <$> localTime <*> (fromMaybe utc <$> timeZone)

utc :: Local.TimeZone
utc = Local.TimeZone 0 False ""

decimal :: (DateParsing m, Integral a) => m a
decimal = foldl' step 0 `fmap` some digit
  where step a w = a * 10 + fromIntegral (ord w - 48)

peekChar :: DateParsing m => m (Maybe Char)
peekChar = optional peekChar'

peekChar' :: DateParsing m => m Char
peekChar' = lookAhead anyChar
