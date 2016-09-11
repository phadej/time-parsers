{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Bifunctor (first)
import Data.Time      (Day (..), UTCTime (..))

import qualified Data.Attoparsec.Text as AT
import qualified Data.Text            as T
import qualified Text.Parsec          as Parsec

import Data.Time.Parsers
import Data.Time.TH

main :: IO ()
main = defaultMain $ testGroup "tests" [utctimeTests, timeTHTests]

utctimeTests :: TestTree
utctimeTests = testGroup "utcTime" $ map t timeStrings
  where
    t str = testCase str $ do
        assertBool str (isRight $ parseParsec str)
        assertEqual str (parseParsec str) (parseAttoParsec str)

isRight :: Either a b -> Bool
isRight (Left _)  = False
isRight (Right _) = True

parseParsec :: String -> Either String UTCTime
parseParsec input = first show $ Parsec.parse utcTime"" input

parseAttoParsec :: String -> Either String UTCTime
parseAttoParsec = AT.parseOnly utcTime . T.pack

timeStrings :: [String]
timeStrings =
    [ "2015-09-07T08:16:40.807Z"
    , "2015-09-07T11:16:40.807+0300"
    , "2015-09-07 08:16:40.807Z"
    , "2015-09-07 08:16:40.807 Z"
    , "2015-09-07 08:16:40.807 +0000"
    , "2015-09-07 08:16:40.807 +00:00"
    , "2015-09-07 11:16:40.807 +03:00"
    , "2015-09-07 05:16:40.807 -03:00"
    , "2015-09-07 05:16:40.807-03:00"
    , "2015-09-07T05:16:40Z"
    , "2015-09-07 05:16:40Z"
    , "2015-09-07 05:16:40 Z"
    , "2015-09-07 05:16:40+03:00"
    , "2015-09-07 05:16:40 +03:00"
    ]

timeTHTests :: TestTree
timeTHTests = testGroup "TH"
    [ testCase "time" $ assertBool "should be equal" $ lhs == rhs
    , testCase "day"  $ assertBool "should be equal" $ lhs' == rhs'
    ]
  where
    lhs  = UTCTime (ModifiedJulianDay 56789) 123.456
    rhs  = $(mkUTCTime "2014-05-12 00:02:03.456000Z")
    lhs' = ModifiedJulianDay 56789
    rhs' = $(mkDay "2014-05-12")

