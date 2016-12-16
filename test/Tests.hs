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
main = defaultMain $ testGroup "tests" [utctimeTests, monthTests, timeTHTests]

utctimeTests :: TestTree
utctimeTests = testGroup "utcTime" $ map t utctimeStrings
  where
    t str = testCase str $ do
        assertBool str (isRight $ parseParsec str)
        assertEqual str (parseParsec str) (parseAttoParsec str)

monthTests :: TestTree
monthTests = testGroup "month" $ 
    [ testGroup "valid" $ map t monthStrings
    , testGroup "invalid" $ map i invalidMonthStrings
    ]
  where
    t str = testCase str $ do
        assertBool str (isRight $ parseParsecMonth str)
        assertEqual str (parseParsecMonth str) (parseAttoParsecMonth str)

    i str = testCase str $
        assertBool str (isLeft $ parseParsecMonth str)

isRight :: Either a b -> Bool
isRight (Left _)  = False
isRight (Right _) = True

isLeft :: Either a b -> Bool
isLeft = not . isRight

parseParsec :: String -> Either String UTCTime
parseParsec input = first show $ Parsec.parse utcTime "" input

parseAttoParsec :: String -> Either String UTCTime
parseAttoParsec = AT.parseOnly utcTime . T.pack

parseParsecMonth :: String -> Either String (Integer, Int)
parseParsecMonth input = first show $ Parsec.parse month "" input

parseAttoParsecMonth :: String -> Either String (Integer, Int)
parseAttoParsecMonth = AT.parseOnly month . T.pack

utctimeStrings :: [String]
utctimeStrings =
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

monthStrings :: [String]
monthStrings =
    [ "2016-12"
    , "-0010-12"
    ]

invalidMonthStrings :: [String]
invalidMonthStrings =
    [ "2016-13"
    , "2016-00"
    , "2016-1"
    ]

timeTHTests :: TestTree
timeTHTests = testGroup "TH"
    [ testCase "time"   $ assertBool "should be equal" $ lhs0 == rhs0
    , testCase "time' " $ assertBool "should be equal" $ lhs1 == rhs1
    , testCase "day"    $ assertBool "should be equal" $ lhs2 == rhs2
    ]
  where
    lhs0 = UTCTime (ModifiedJulianDay 56789) 123.456
    rhs0 = $(mkUTCTime "2014-05-12 00:02:03.456Z")
    lhs1 = UTCTime (ModifiedJulianDay 56789) 123.0
    rhs1 = $(mkUTCTime "2014-05-12 00:02:03Z")
    lhs2 = ModifiedJulianDay 56789
    rhs2 = $(mkDay "2014-05-12")

