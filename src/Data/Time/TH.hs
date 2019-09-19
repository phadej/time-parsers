{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif
-- | Template Haskell extras for `Data.Time`.
module Data.Time.TH (mkUTCTime, mkDay) where

import Data.List                    (nub)
import Data.Time                    (Day (..), UTCTime (..))
import Data.Time.Parsers            (day, utcTime)
import Language.Haskell.TH          (Exp, Q, integerL, litE, appE, sigE, rationalL)
import Text.ParserCombinators.ReadP (readP_to_S)

-- | Make  a 'UTCTime'. Accepts the same strings as  `utcTime` parser accepts.
--
-- > t :: UTCTime
-- > t = $(mkUTCTime "2014-05-12 00:02:03.456000Z")
mkUTCTime :: String -> Q Exp
mkUTCTime s = case nub $ readP_to_S utcTime s of
    [(UTCTime (ModifiedJulianDay d) dt, "")] ->
        ([| UTCTime |] `appE` ([| ModifiedJulianDay |] `appE` d') `appE` dt') `sigE` [t| UTCTime |]
      where
        d'  = litE $ integerL d
        dt' = litE $ rationalL $ toRational dt
    ps -> error $ "Cannot parse date: " ++ s ++ " -- " ++ show ps

-- | Make  a 'Day'. Accepts the same strings as  `day` parser accepts.
--
-- > d :: Day
-- > d = $(mkDay "2014-05-12")
mkDay :: String -> Q Exp
mkDay s = case nub $ readP_to_S day s of
    [(ModifiedJulianDay d, "")] ->
        ([| ModifiedJulianDay |] `appE` d') `sigE` [t| Day |]
      where
        d'  = litE $ integerL d
    ps -> error $ "Cannot parse day: " ++ s ++ " -- " ++ show ps
