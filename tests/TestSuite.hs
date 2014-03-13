{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import Parsing.Chronic
import Helpers

import Test.Framework (defaultMain)
import           Test.Framework                     (Test, testGroup)
import           Test.Framework.Providers.HUnit     (testCase)
import           Test.HUnit                         ((@=?), assertBool, assertFailure)
import qualified Data.Time as DT



simpleComparisonCase name format time = testCase name $
  Right (timeLiteral format time) @=? parserUnderTest time


data ActualTime = ActualTime { unActualTime :: DT.UTCTime }
actualTime = ActualTime

data TestTime  = TestTime { unTestTime :: String }
testTime = TestTime

data ParserOptions = ParserOptions { unParserOptions :: [ChronicOptions -> ChronicOptions] }
parserOptions = ParserOptions

data CurrentTime = CurrentTime { unCurrentTime :: DT.UTCTime }
currentTime = CurrentTime

monadicComaprisonCase name actual time now opts =
  testCase name $ 
        Right (unActualTime actual) @=?
        runChronicTest (unCurrentTime now) (parserUnderTestMOpts (unParserOptions opts) (unTestTime time))

main :: IO ()
main = defaultMain
    [ testHandleGeneric
    ]

testHandleGeneric :: Test
testHandleGeneric = testGroup "test_handle_generic"
    [ testCase "0" $ (\time -> 
        Right (timeLiteral (fmt "%FT%T") time) @=? parserUnderTest time
      ) "2012-08-02T13:00:00"

    , simpleComparisonCase "1" (fmt "%FT%T") "2012-08-02T13:00:00"

    , simpleComparisonCase "2" (fmt "%FT%T%z") "2012-08-02T13:00:00+01:00"
        
    , simpleComparisonCase "3" (fmt "%FT%T%z") "2012-08-02T08:00:00-04:00"

      {- original case
      time = Chronic.parse("2013-08-01T19:30:00.345-07:00")
      time2 = Time.parse("2013-08-01 019:30:00.345-07:00")
      assert_in_delta time, time2, 0.001
      -}
    , simpleComparisonCase "4" (fmt "%FT%T%Q%z") "2013-08-01T19:30:00.345-07:00"

    , simpleComparisonCase "5" (fmt "%FT%TZ") "2012-08-02T12:00:00Z"

      {- original case
      time = Chronic.parse("2012-01-03 01:00:00.100")
      time2 = Time.parse("2012-01-03 01:00:00.100")
      assert_in_delta time, time2, 0.001
      -}
    , simpleComparisonCase "6" (fmt "%F %T%Q") "2012-01-03 01:00:00.100"

      {- original case
      time = Chronic.parse("2012-01-03 01:00:00.234567")
      time2 = Time.parse("2012-01-03 01:00:00.234567")
      assert_in_delta time, time2, 0.000001
      -}
    , simpleComparisonCase "7" (fmt "%F %T%Q") "2012-01-03 01:00:00.234567"

    , testCase "8" $ case(parserUnderTest "1/1/32.1") of
        Right _ -> assertFailure "unexpectedly parsed"
        _       -> assertBool "" True

    , testCase "9" $ (\now ->
        Right (timeLiteral (fmt "%F") "2014-03-28") @=?
        runChronicTest now (parserUnderTestMOpts [guess Begin] "28th")
      ) $ timeLiteral (fmt "%F") "2014-03-10"

    , monadicComaprisonCase "10"
          (actualTime (timeLiteral (fmt "%F") "2014-03-28"))
          (testTime   "28th")
          (currentTime (timeLiteral (fmt "%F") "2014-03-10"))
          (parserOptions [])
    ]

{-
testHandleRmnSd :: Test
testHandleRmnSd = testGroup "test_handle_rmnd_sd"
    [ testCase "0
-}
