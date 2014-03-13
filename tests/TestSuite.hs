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

chronicNowTime =  timeLiteral (fmt "%F%T") "2006-8-16:14:00:00"

main :: IO ()
main = defaultMain
    [ testHandleGeneric
    , testHandleRmnSd
    , testHandleRmnSdOn
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

    , monadicComaprisonCase "9"
          (actualTime (timeLiteral (fmt "%F") "2014-03-28"))
          (testTime   "28th")
          (currentTime (timeLiteral (fmt "%F") "2014-03-10"))
          (parserOptions [])
    ]

testHandleRmnSd :: Test
testHandleRmnSd = testGroup "test_handle_rmnd_sd"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-03 12:00:00"))
        (testTime   "aug 3")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-03 12:00:00"))
        (testTime   "aug 3")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-03 12:00:00"))
        (testTime   "aug. 3")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-20 12:00:00"))
        (testTime   "aug 20")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-20 12:00:00"))
        (testTime   "aug-20")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "6"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-20 12:00:00"))
        (testTime   "aug 20")
        (currentTime chronicNowTime)
        (parserOptions [context Future])

    , monadicComaprisonCase "7"
        (actualTime (timeLiteral (fmt "%F %T") "2007-05-27 12:00:00"))
        (testTime   "may 27")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "8"
        (actualTime (timeLiteral (fmt "%F %T") "2006-05-28 12:00:00"))
        (testTime   "may 28")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "9"
        (actualTime (timeLiteral (fmt "%F %T") "2006-05-28 17:00:00"))
        (testTime   "may 28 5pm")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "10"
        (actualTime (timeLiteral (fmt "%F %T") "2006-05-28 17:00:00"))
        (testTime   "may 28 5pm")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "10"
        (actualTime (timeLiteral (fmt "%F %T") "2006-05-28 17:00:00"))
        (testTime   "may 28 at 5pm")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "10"
        (actualTime (timeLiteral (fmt "%F %T") "2006-05-28 17:32:19"))
        (testTime   "may 28 at 5:32.19pm")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "10"
        (actualTime (timeLiteral (fmt "%F %T") "2006-05-28 17:32:19"))
        (testTime   "may 28 at 5:32.19pm")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

      {- original case
      time = parse_now("may 28 at 5:32:19.764")
      assert_in_delta Time.local(2007, 5, 28, 17, 32, 19, 764000), time, 0.001
      -}
    , monadicComaprisonCase "11"
        (actualTime (timeLiteral (fmt "%F %T%Q") "2006-05-28 17:32:19.764"))
        (testTime   "may 28 at 5:32:19.764")
        (currentTime chronicNowTime)
        (parserOptions [context Past])
    ]


testHandleRmnSdOn :: Test
testHandleRmnSdOn = testGroup "test_handle_rmnd_sd"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2007-05-28 17:00:00"))
        (testTime   "5pm on may 28")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2007-05-28 17:00:00"))
        (testTime   "5pm may 28")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2007-05-28 05:00:00"))
        (testTime   "5 on may 28")
        (currentTime chronicNowTime)
        (parserOptions [ambiguousTimeRange Nothing])
    ]

{-

    time = parse_now("5 on may 28", :ambiguous_time_range => :none)
    assert_equal Time.local(2007, 5, 28, 05), time
  end

  def test_handle_rmn_od
    time = parse_now("may 27th")
    assert_equal Time.local(2007, 5, 27, 12), time

    time = parse_now("may 27th", :context => :past)
    assert_equal Time.local(2006, 5, 27, 12), time

    time = parse_now("may 27th 5:00 pm", :context => :past)
    assert_equal Time.local(2006, 5, 27, 17), time

    time = parse_now("may 27th at 5pm", :context => :past)
    assert_equal Time.local(2006, 5, 27, 17), time

    time = parse_now("may 27th at 5", :ambiguous_time_range => :none)
    assert_equal Time.local(2007, 5, 27, 5), time
  end

  def test_handle_od_rm
    time = parse_now("fifteenth of this month")
    assert_equal Time.local(2006, 8, 15, 12), time
  end

  def test_handle_od_rmn
    time = parse_now("22nd February")
    assert_equal Time.local(2007, 2, 22, 12), time

    time = parse_now("31st of may at 6:30pm")
    assert_equal Time.local(2007, 5, 31, 18, 30), time

    time = parse_now("11th december 8am")
    assert_equal Time.local(2006, 12, 11, 8), time
  end
  -}
