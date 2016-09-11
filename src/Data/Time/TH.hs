{-# LANGUAGE TemplateHaskell #-}
-- | Template Haskell extras for `Data.Time`.
module Data.Time.TH (mkUTCTime, mkDay) where

import Data.Time                    (Day (..), UTCTime (..))
import Data.Time.Parsers            (day, utcTime)
import Language.Haskell.TH          (Exp, Q, integerL, litE, rationalL)
import Text.ParserCombinators.ReadP (readP_to_S)

-- | Make  a 'UTCTime'. Accepts the same strings as  `utcTime` parser accepts.
--
-- > t :: UTCTime
-- > t = $(mkUTCTime "2014-05-12 00:02:03.456000Z")
mkUTCTime :: String -> Q Exp
mkUTCTime s = case readP_to_S utcTime s of
    [(UTCTime (ModifiedJulianDay d) dt, "")] ->
        [| UTCTime (ModifiedJulianDay $(d')) $(dt') :: UTCTime |]
      where
        d'  = litE $ integerL d
        dt' = litE $ rationalL $ toRational dt
    _ -> error $ "Cannot parse date: " ++ s

-- | Make  a 'Day'. Accepts the same strings as  `day` parser accepts.
--
-- > d :: Day
-- > d = $(mkDay "2014-05-12")
mkDay :: String -> Q Exp
mkDay s = case readP_to_S day s of
    [(ModifiedJulianDay d, "")] ->
        [| ModifiedJulianDay $(d') :: Day |]
      where
        d'  = litE $ integerL d
    _ -> error $ "Cannot parse day: " ++ s
