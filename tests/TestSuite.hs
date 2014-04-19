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
import           Data.Time.Calendar(toGregorian)
import Data.Monoid((<>))
import qualified Data.Time as DT



simpleComparisonCase name format opts time = testCase name $
  Right (timeLiteral format time) @=? parserUnderTest opts time


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
        runChronicTest (unCurrentTime now) (parserUnderTestM (unParserOptions opts) (unTestTime time))

monadicNilCase name time now opts =
  testCase name $
        case (runChronicTest (unCurrentTime now) (parserUnderTestM (unParserOptions opts) (unTestTime time))) of
          Right _ -> assertFailure "unexpectedly parsed"
          _       -> assertBool "" True

    

chronicNowTime =  timeLiteral (fmt "%F%T") "2006-8-16:14:00:00"

main :: IO ()
main = defaultMain
    [ testHandleGeneric
    , testHandleRmnSd
    , testHandleRmnSdOn
    , testHandleRmnOd
    , testHandleOdRm
    , testHandleOdRmn
    , testHandleSyRmnOd 
    , testHandleSdRmn 
    , testHandleRmnOdOn 
    , testHandleRmnSy 
    , testHandleRdnRmnDsTTzSy 
    , testHandleSySmSdTTz
    , testHandleRmnSdSy 
    , testHandleRmnOdSy 
    , testHandleOdRmnSy 
    , testHandleSdRmnSy 
    , testHandleSmSdSy 
    , testHandleSdSmSy 
    , testHandleSySmSd 
    , testHandleSmSd 
    , testHandleSySm
    , testHandleR
    , testHandleSRPA 
    , testHandleOrr 
    , testHandleORSR 
    , testHandleORGR
    , testHandleSmRmnSy 
    , testParseGuessR
    , testParseGuessRR
    , testParseGuessRRR
    , testParseGuessGR
    ]

{-- In the Chronic source, 
- r  <-> repeater
- s  <-> scalar
- mn <-> month name
- on <-> seperator on
- od <-> ordinal day
-
--}

testHandleGeneric :: Test
testHandleGeneric = testGroup "test_handle_generic"
    [ testCase "0" $ (\time -> 
        Right (timeLiteral (fmt "%FT%T") time) @=? parserUnderTest [] time
      ) "2012-08-02T13:00:00"

    , simpleComparisonCase "1" (fmt "%FT%T") [] "2012-08-02T13:00:00"

    , simpleComparisonCase "2" (fmt "%FT%T%z") [] "2012-08-02T13:00:00+01:00"
        
    , simpleComparisonCase "3" (fmt "%FT%T%z") [] "2012-08-02T08:00:00-04:00"

      {- original case
      time = Chronic.parse("2013-08-01T19:30:00.345-07:00")
      time2 = Time.parse("2013-08-01 019:30:00.345-07:00")
      assert_in_delta time, time2, 0.001
      -}
    , simpleComparisonCase "4" (fmt "%FT%T%Q%z") [] "2013-08-01T19:30:00.345-07:00"

    , simpleComparisonCase "5" (fmt "%FT%TZ") [] "2012-08-02T12:00:00Z"

      {- original case
      time = Chronic.parse("2012-01-03 01:00:00.100")
      time2 = Time.parse("2012-01-03 01:00:00.100")
      assert_in_delta time, time2, 0.001
      -}
    , simpleComparisonCase "6" (fmt "%F %T%Q") [] "2012-01-03 01:00:00.100"

      {- original case
      time = Chronic.parse("2012-01-03 01:00:00.234567")
      time2 = Time.parse("2012-01-03 01:00:00.234567")
      assert_in_delta time, time2, 0.000001
      -}
    , simpleComparisonCase "7" (fmt "%F %T%Q") [] "2012-01-03 01:00:00.234567"

    , testCase "8" $ case(parserUnderTest [] "1/1/32.1") of
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
testHandleRmnSdOn = testGroup "test_handle_rmnd_sd_on"
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

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2007-05-28 05:00:00"))
        (testTime   "5 on may 28")
        (currentTime chronicNowTime)
        (parserOptions [ambiguousTimeRange Nothing])
    ]

testHandleRmnOd :: Test
testHandleRmnOd = testGroup "test_handle_rmnd_od"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2007-05-27 12:00:00"))
        (testTime   "may 27th")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2007-05-27 12:00:00"))
        (testTime   "may 27th")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2006-05-27 17:00:00"))
        (testTime   "may 27th 5:00 pm")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2006-05-27 17:00:00"))
        (testTime   "may 27th at 5pm")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T") "2006-05-27 05:00:00"))
        (testTime   "may 27th at 5")
        (currentTime chronicNowTime)
        (parserOptions [ambiguousTimeRange Nothing])
    ]

testHandleOdRm :: Test
testHandleOdRm = testGroup "test_handle_od_rm"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-15 12:00:00"))
        (testTime   "fifteenth of this month")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]

testHandleOdRmn :: Test
testHandleOdRmn = testGroup "test_handle_od_rmn"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2007-02-22 12:00:00"))
        (testTime   "22nd February")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2007-05-31 18:30:00"))
        (testTime   "31st of may at 6:30pm")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2006-12-11 08:00:00"))
        (testTime   "11th december 8am")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]

testHandleSyRmnOd :: Test
testHandleSyRmnOd = testGroup "test_handle_sy_rmn_od"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2009-05-22 12:00:00"))
        (testTime   "2009 May 22nd")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]

testHandleSdRmn :: Test
testHandleSdRmn = testGroup "test_handle_sd_rmn"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2007-02-22 12:00:00"))
        (testTime   "22 February")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2007-02-22 12:00:00"))
        (testTime   "22 feb")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2007-02-22 12:00:00"))
        (testTime   "22-feb")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2007-05-31 18:30:00"))
        (testTime   "31 of may at 6:30pm")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T") "2006-12-11 08:00:00"))
        (testTime   "11 december 8am")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]

testHandleRmnOdOn :: Test
testHandleRmnOdOn = testGroup "test_handle_rmn_od_on"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2006-05-27 17:00:00"))
        (testTime   "5:00 pm may 27th")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2006-05-27 17:00:00"))
        (testTime   "05:00 pm may 27th")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2006-05-27 17:00:00"))
        (testTime   "5pm on may 27th")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2007-05-27 05:00:00"))
        (testTime   "5 on may 27th")
        (currentTime chronicNowTime)
        (parserOptions [ambiguousTimeRange Nothing])
    ]

testHandleRmnSy :: Test
testHandleRmnSy = testGroup "test_handle_rmn_sy"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "1997-05-16 12:00:00"))
        (testTime   "may 97")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2033-05-16 12:00:00"))
        (testTime   "may 33")
        (currentTime chronicNowTime)
        (parserOptions [ambiguousYearFutureBias 10])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2032-05-16 12:00:00"))
        (testTime   "may 32")
        (currentTime chronicNowTime)
        (parserOptions [ambiguousYearFutureBias 10])
    ]

{- original cases: 
- ...
- assert_equal ..., time.to_i
-}
testHandleRdnRmnDsTTzSy :: Test
testHandleRdnRmnDsTTzSy = testGroup "test_handle_rdn_rmn_ds_t_tz_sy"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T %Z") "2007-01-02 17:00:00 PDT"))
        (testTime   "Mon Apr 02 17:00:00 PDT 2007")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]

{- original cases: 
- ...
- assert_equal ..., time.to_i 
- for test 1,2,3,4
-
- and
-
- assert_delta ..., time.to_f, 0.001
- for test 5
-}
testHandleSySmSdTTz :: Test
testHandleSySmSdTTz = testGroup "test_handle_sy_sm_sd_t_tz"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T %z") "2011-07-03 22:11:35 +0100"))
        (testTime   "2011-07-03 22:11:35 +0100")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T %z") "2011-07-03 22:11:35 +0100"))
        (testTime   "2011-07-03 22:11:35 +01:00")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T %z") "2011-07-03 16:11:35 -0500"))
        (testTime   "2011-07-03 16:11:35 -05:00")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T UTC") "2011-07-03 21:11:35 UTC"))
        (testTime   "2011-07-03 21:11:35 UTC")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T%Q UTC") "2011-07-03 21:11:35.362 UTC"))
        (testTime   "2011-07-03 21:11:35.362 UTC")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]

testHandleRmnSdSy :: Test
testHandleRmnSdSy = testGroup "test_handle_rmn_sd_sy"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F") "2010-11-18"))
        (testTime   "November 18, 2010")
        (currentTime chronicNowTime)
        (parserOptions [])
        
    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2010-11-18 12:00:00"))
        (testTime   "November 18, 2010")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2004-02-14 12:00:00"))
        (testTime   "February 14, 2004")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2010-01-03 12:00:00"))
        (testTime   "jan 3 2010")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2010-01-03 12:00:00"))
        (testTime   "jan 3 2010")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T") "2010-01-04 00:00:00"))
        (testTime   "jan 3 2010 midnight")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "6"
        (actualTime (timeLiteral (fmt "%F %T") "2010-01-03 04:00:00"))
        (testTime   "jan 3 2010 at 4")
        (currentTime chronicNowTime)
        (parserOptions [ambiguousTimeRange Nothing])

    , monadicComaprisonCase "7"
        (actualTime (timeLiteral (fmt "%F %T") "1979-05-12 05:00:00"))
        (testTime   "may 27, 1979")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "8"
        (actualTime (timeLiteral (fmt "%F %T") "1979-05-12 05:00:00"))
        (testTime   "may 27 79")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "9"
        (actualTime (timeLiteral (fmt "%F %T") "1979-05-27 16:30:00"))
        (testTime   "may 27 79 4:30")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "9"
        (actualTime (timeLiteral (fmt "%F %T") "1979-05-27 04:30:00"))
        (testTime   "may 27 79 at 4:30")
        (currentTime chronicNowTime)
        (parserOptions [ambiguousTimeRange Nothing])

    , monadicComaprisonCase "10"
        (actualTime (timeLiteral (fmt "%F %T") "2032-05-27 12:00:00"))
        (testTime   "may 27 32")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "11"
        (actualTime (timeLiteral (fmt "%F %T") "2012-10-05 22:45:00"))
        (testTime   "oct 5 2012 1045pm")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]
testHandleRmnOdSy :: Test
testHandleRmnOdSy = testGroup "test_handle_rmn_od_sy"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2001-05-01 12:00:00"))
        (testTime   "may 1st 01")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2010-11-18 12:00:00"))
        (testTime   "November 18th 2010")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2010-11-18 12:00:00"))
        (testTime   "November 18th, 2010")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2010-11-19 00:00:00"))
        (testTime   "November 18th 2010 midnight")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T") "2010-11-19 00:00:00"))
        (testTime   "November 18th 2010 at midnight")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "6"
        (actualTime (timeLiteral (fmt "%F %T") "2010-11-18 16:00:00"))
        (testTime   "November 18th 2010 at 4")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "7"
        (actualTime (timeLiteral (fmt "%F %T") "2010-11-18 04:00:00"))
        (testTime   "November 18th 2010 at 4")
        (currentTime chronicNowTime)
        (parserOptions [ambiguousTimeRange Nothing])

    , monadicComaprisonCase "8"
        (actualTime (timeLiteral (fmt "%F %T") "1979-03-30 12:00:00"))
        (testTime   "March 30th, 1979")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "9"
        (actualTime (timeLiteral (fmt "%F %T") "1979-03-30 12:00:00"))
        (testTime   "March 30th 79")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "10"
        (actualTime (timeLiteral (fmt "%F %T") "1979-03-30 16:30:00"))
        (testTime   "March 30th 79 4:30")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "11"
        (actualTime (timeLiteral (fmt "%F %T") "1979-03-30 04:30:00"))
        (testTime   "March 30th 79 at 4:30")
        (currentTime chronicNowTime)
        (parserOptions [ambiguousTimeRange Nothing])
    ]
testHandleOdRmnSy :: Test
testHandleOdRmnSy = testGroup "test_handle_od_rmn_sy"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2012-02-22 12:00:00"))
        (testTime   "22nd February 2012")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "1979-12-11 12:00:00"))
        (testTime   "11th december 79")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]

testHandleSdRmnSy :: Test
testHandleSdRmnSy = testGroup "test_handle_sd_rmn_sy"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2010-01-03 12:00:00"))
        (testTime   "3 jan 2010")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2010-01-03 16:00:00"))
        (testTime   "3 jan 2010 4pm")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2006-10-27 19:30:00"))
        (testTime   "27 Oct 2006 7:30pm")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]

testHandleSmSdSy :: Test
testHandleSmSdSy = testGroup "test_handle_sm_sd_sy"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "1979-05-27 12:00:00"))
        (testTime   "5/27/1979")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "1979-05-27 04:00:00"))
        (testTime   "5/27/1979 4am")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2011-07-12 12:00:00"))
        (testTime   "7/12/11")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2011-12-07 12:00:00"))
        (testTime   "7/12/11")
        (currentTime chronicNowTime)
        (parserOptions [endianPrecedence LittleMiddle])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T") "2011-09-19 18:05:57"))
        (testTime   "9/19/2011 6:05:57 PM")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T") "2011-09-19 18:05:57"))
        (testTime   "9/19/2011 6:05:57 PM")
        (currentTime chronicNowTime)
        (parserOptions [])

    -- month day overflows, no such date Feb. 30
    , monadicNilCase "5"
        (testTime   "30/2/2000")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "6"
        (actualTime (timeLiteral (fmt "%F %T") "2013-03-12 17:00:00"))
        (testTime   "2013-03-12 17:00")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    ]

testHandleSdSmSy :: Test
testHandleSdSmSy = testGroup "test_handle_sd_sm_sy"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "1979-05-27 17:00:00"))
        (testTime   "27/5/1979")
        (currentTime chronicNowTime)
        (parserOptions [])
     
    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "1979-05-27 07:00:00"))
        (testTime   "27/5/1979 @ 700")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "1979-05-27 07:00:00"))
        (testTime   "18/3/2012 @ 700")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2012-03-18 17:26:00"))
        (testTime   "03/18/2012 09:26 pm")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T") "2013-07-30 16:34:22"))
        (testTime   "30.07.2013 16:34:22")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "6"
        (actualTime (timeLiteral (fmt "%F %T") "2013-09-08 12:00:00"))
        (testTime   "09.08.2013")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "7"
        (actualTime (timeLiteral (fmt "%F %T") "2013-07-30 21:53:49"))
        (testTime   "30-07-2013 21:53:49")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]
testHandleSySmSd :: Test
testHandleSySmSd = testGroup "test_handle_sy_sm_sd"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2000-01-01 12:00:00"))
        (testTime   "2000-1-1")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-20 12:00:00"))
        (testTime   "2006-08-20")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-20 19:00:00"))
        (testTime   "2006-08-20 7pm")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-20 03:00:00"))
        (testTime   "2006-08-20 03:00")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-20 03:30:30"))
        (testTime   "2006-08-20 03:30:30")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "6"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-20 15:30:30"))
        (testTime   "2006-08-20 15:30:30")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "7"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-20 15:30:30"))
        (testTime   "2006-08-20 15:30.30")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "8"
        (actualTime (timeLiteral (fmt "%F %T%Q") "2006-08-20 15:30:30.000536"))
        (testTime   "2006-08-20 15:30.30:000536")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "9"
        (actualTime (timeLiteral (fmt "%F %T") "1902-08-20 12:00:00"))
        (testTime   "1902-08-20")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "10"
        (actualTime (timeLiteral (fmt "%F %T") "2013-07-30 11:45:23"))
        (testTime   "2013.07.30 11:45:23")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "11"
        (actualTime (timeLiteral (fmt "%F %T") "2013-07-30 11:45:23"))
        (testTime   "2013.07.30 11:45:23")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "12"
        (actualTime (timeLiteral (fmt "%F %T") "2013-08-09 12:00:00"))
        (testTime   "2013.08.09")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "13"
        (actualTime (timeLiteral (fmt "%F %T") "2013-08-09 12:00:00"))
        (testTime   "2013.08.09")
        (currentTime chronicNowTime)
        (parserOptions [])

    -- exif date time original (?)
    , monadicComaprisonCase "14"
        (actualTime (timeLiteral (fmt "%F %T") "2012-05-25 22:06:50"))
        (testTime   "2012:05:25 22:06:50")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]


testHandleSmSd :: Test
testHandleSmSd = testGroup "test_handle_sm_sd"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2007-05-06 12:00:00"))
        (testTime   "05/06")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2007-06-05 12:00:00"))
        (testTime   "05/06")
        (currentTime chronicNowTime)
        (parserOptions [endianPrecedence LittleMiddle])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2007-06-05 18:05:57"))
        (testTime   "05/06 6:05:57 PM")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2007-05-06 18:05:57"))
        (testTime   "05/06 6:05:57 PM")
        (currentTime chronicNowTime)
        (parserOptions [endianPrecedence LittleMiddle])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T") "2006-09-13 12:00:00"))
        (testTime   "13/09")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "6"
        (actualTime (timeLiteral (fmt "%F %T") "2007-05-06 12:00:00"))
        (testTime   "05/06")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "7"
        (actualTime (timeLiteral (fmt "%F %T") "2007-01-13 12:00:00"))
        (testTime   "1/13")
        (currentTime chronicNowTime)
        (parserOptions [context Future])

    , monadicComaprisonCase "8"
        (actualTime (timeLiteral (fmt "%F %T") "2006-03-13 12:00:00"))
        (testTime   "3/13")
        (currentTime chronicNowTime)
        (parserOptions [context None])
    ]

testHandleSySm :: Test
testHandleSySm = testGroup "test_handle_sy_sm"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2012-06-16 12:00:00"))
        (testTime   "2102-06")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2013-12-16 12:00:00"))
        (testTime   "2013/12")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]

testHandleR :: Test
testHandleR = testGroup "test_handle_r"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-19 09:00:00"))
        (testTime   "9am on Saturday")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-22 12:00:00"))
        (testTime   "on Tuesday")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 13:00:00"))
        (testTime   "1:00:00 PM")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 14:00:00"))
        (testTime   "today at 2:00:00")
        (currentTime chronicNowTime)
        (parserOptions [hours24 False])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 02:00:00"))
        (testTime   "today at 2:00:00 AM")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "6"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 02:00:00"))
        (testTime   "today at 2:00:00 AM")
        (currentTime chronicNowTime)
        (parserOptions [hours24 False])
     
    , monadicComaprisonCase "7"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 03:00:00"))
        (testTime   "today at 3:00:00")
        (currentTime chronicNowTime)
        (parserOptions [hours24 True])

    , monadicComaprisonCase "8"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-17 04:00:00"))
        (testTime   "tommorrow at 4a.m.")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]

testHandleSRPA :: Test
testHandleSRPA = testGroup "test_handle_s_r_p_a"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-14 00:00:00"))
        (testTime   "two days ago 0:0:0am")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-14 00:00:00"))
        (testTime   "two days ago 00:00:00am")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]

testHandleOrr :: Test
testHandleOrr = testGroup "test_handle_orr" (
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2007-01-30 12:00:00"))
        (testTime   "5th tuesday in january")
        (currentTime chronicNowTime)
        (parserOptions [])

    , testCase "2" $ case(parserUnderTest [] "5th tuesday in february") of
        Right _ -> assertFailure "unexpectedly parsed"
        _       -> assertBool "" True
    ]

    <>
    
    --TODO: refactor into func
    do {
      month <- zip ["jan", "feb", "march", "april", "may", "june", "july", "aug", "sep"]  [1..9];
      [
        testCase (show (snd month)) $ 
          Right (snd month) @=? do
            time <- runChronicTest chronicNowTime (parserUnderTestM []  ("5th tuesday in " <> (fst month))) 
            let (_, month, _) = (toGregorian . DT.utctDay) time
            return month
      ];
    }

    <>

    do {
      month <- zip ["oct", "nov", "dec"]  [10..12];
      [
        testCase (show (snd month)) $ 
          Right (snd month) @=? do
            time <- runChronicTest chronicNowTime (parserUnderTestM []  ("5th tuesday in " <> (fst month))) 
            let (_, month, _) = (toGregorian . DT.utctDay) time
            return month
      ];
    }
   )

testHandleORSR :: Test
testHandleORSR = testGroup "test_handle_o_r_s_r" 
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2006-11-15 12:00:00"))
        (testTime   "3rd wednesday in november")
        (currentTime chronicNowTime)
        (parserOptions [])

    , testCase "2" $ case(parserUnderTest [] "10th wednesday in november") of
        Right _ -> assertFailure "unexpectedly parsed"
        _       -> assertBool "" True

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2007-01-20 12:00:00"))
        (testTime   "3rd wednesday in 2007")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]
     
testHandleORGR :: Test
testHandleORGR = testGroup "test_handle_o_r_g_r" 
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2007-03-01 00:00:00"))
        (testTime   "3rd month next year")
        (currentTime chronicNowTime)
        (parserOptions [guess (Guess False)])

    --skip test 2, not relevant
     
    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2006-09-21 12:00:00"))
        (testTime   "3rd thursday this september")
        (currentTime chronicNowTime)
        (parserOptions [])

    -- skip test 3, not relevant

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-09 12:00:00"))
        (testTime   "4th day last week")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]

testHandleSmRmnSy :: Test
testHandleSmRmnSy  = testGroup "test_handle_sm_rmn_sy" 
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2011-03-30 12:00:00"))
        (testTime   "30-Mar-11")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2012-08-31 12:00:00"))
        (testTime   "31-Aug-12")
        (currentTime chronicNowTime)
        (parserOptions [])

    ]

testParseGuessR :: Test
testParseGuessR  = testGroup "test_parse_guess_r" 
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-18 12:00:00"))
        (testTime   "friday")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-22 12:00:00"))
        (testTime   "tue")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 17:00:00"))
        (testTime   "5")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 17:00:00"))
        (testTime   "5")
        (currentTime (timeLiteral (fmt "%F %R") "2006-08-16 03:00:00"))
        (parserOptions [ambiguousTimeRange Nothing])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-17 13:00:00"))
        (testTime   "13:00")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "6"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-17 13:45:00"))
        (testTime   "13:45")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "7"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 13:01:00"))
        (testTime   "1:01pm")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "8"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 14:01:00"))
        (testTime   "2:01pm")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "8"
        (actualTime (timeLiteral (fmt "%F %T") "2006-11-16 12:00:00"))
        (testTime   "november")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]

testParseGuessRR :: Test
testParseGuessRR  = testGroup "test_parse_guess_rr"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-18 13:00:00"))
        (testTime   "friday 13:00")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-21 16:00:00"))
        (testTime   "monday 4:00")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-19 04:00:00"))
        (testTime   "sat 4:00")
        (currentTime chronicNowTime)
        (parserOptions [ambiguousTimeRange Nothing])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-20 04:20:00"))
        (testTime   "sunday 4:20")
        (currentTime chronicNowTime)
        (parserOptions [ambiguousTimeRange Nothing])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 16:00:00"))
        (testTime   "4 pm")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "6"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 04:00:00"))
        (testTime   "4 am")
        (currentTime chronicNowTime)
        (parserOptions [ambiguousTimeRange Nothing])

    , monadicComaprisonCase "7"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 12:00:00"))
        (testTime   "12 pm")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "8"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 12:00:01"))
        (testTime   "12:01 pm")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "9"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 00:00:01"))
        (testTime   "12:01 am")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "9"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 00:00:00"))
        (testTime   "12 am")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "10"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 04:00:00"))
        (testTime   "4:00 in the morning")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "11"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-17 00:00:10"))
        (testTime   "0:10")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "12"
        (actualTime (timeLiteral (fmt "%F %T") "2006-11-04 12:00:00"))
        (testTime   "november 4")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "13"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-24 12:00:00"))
        (testTime   "aug 24")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]

testParseGuessRRR :: Test
testParseGuessRRR  = testGroup "test_parse_guess_rrr"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-18 13:00:00"))
        (testTime   "friday 1 pm")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-18 23:00:00"))
        (testTime   "friday 11 at night")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-18 23:00:00"))
        (testTime   "friday 11 in the evening")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-20 06:00:00"))
        (testTime   "sunaday 6am")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-18 19:00:00"))
        (testTime   "friday evening at 7")
        (currentTime chronicNowTime)
        (parserOptions [])
    ]

testParseGuessGR :: Test
testParseGuessGR  = testGroup "test_parse_guess_gr"
    [ monadicComaprisonCase "1"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-17 12:00:00"))
        (testTime   "this year")
        (currentTime chronicNowTime)
        (parserOptions [guess (Guess False)])
         
    , monadicComaprisonCase "2"
        (actualTime (timeLiteral (fmt "%F %T") "2006-01-01 12:00:00"))
        (testTime   "this year")
        (currentTime chronicNowTime)
        (parserOptions [guess (Guess False), context Past])

    , monadicComaprisonCase "3"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-24 12:00:00"))
        (testTime   "this month")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "4"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-08 12:00:00"))
        (testTime   "this month")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "5"
        (actualTime (timeLiteral (fmt "%F %T") "2006-12-16 12:00:00"))
        (testTime   "next monrth")
        (currentTime (timeLiteral (fmt "%F %T") "2006-11-15 12:00:00"))
        (parserOptions [])

    , monadicComaprisonCase "6"
        (actualTime (timeLiteral (fmt "%F %T") "2005-11-16 16:00:00"))
        (testTime   "last november")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "7"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-21 19:30:00"))
        (testTime   "this fortnight")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "7"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-14 19:00:00"))
        (testTime   "this fortnight")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "8"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-18 07:30:00"))
        (testTime   "this week")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "9"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-14 19:00:00"))
        (testTime   "this week")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "10"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-13 12:00:00"))
        (testTime   "this week")
        (currentTime chronicNowTime)
        (parserOptions [context Past, guess Begin])

    , monadicComaprisonCase "11"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-14 12:00:00"))
        (testTime   "this week")
        (currentTime chronicNowTime)
        (parserOptions [context Past, guess Begin, weekStart Monday])

    , monadicComaprisonCase "12"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-20 12:00:00"))
        (testTime   "this weekend")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "13"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-13 12:00:00"))
        (testTime   "this weekend")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "14"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-13 12:00:00"))
        (testTime   "last weekend")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "15"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 19:00:00"))
        (testTime   "this day")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "16"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 07:00:00"))
        (testTime   "this day")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "17"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 19:00:00"))
        (testTime   "today")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "18"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-15 12:00:00"))
        (testTime   "yesterday")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "19"
        (actualTime (timeLiteral (fmt "%F %T") "2011-05-27 23:10:00"))
        (testTime   "yesterday")
        (currentTime (timeLiteral (fmt "%F %T") "2011-05-27 23:10:00"))
        (parserOptions [])

    , monadicComaprisonCase "20"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-17 12:00:00"))
        (testTime   "tommorow")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "21"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-22 12:00:00"))
        (testTime   "this tuesday")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "22"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-22 12:00:00"))
        (testTime   "next tuesday")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "23"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-15 12:00:00"))
        (testTime   "last tuesday")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "24"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-23 12:00:00"))
        (testTime   "this wed")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "25"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-23 12:00:00"))
        (testTime   "next wed")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "26"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-09 12:00:00"))
        (testTime   "last wed")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "27"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-21 12:00:00"))
        (testTime   "mon")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "28"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-21 12:00:00"))
        (testTime   "mun")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "29"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-22 12:00:00"))
        (testTime   "tue")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "30"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-22 12:00:00"))
        (testTime   "tus")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "31"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-23 12:00:00"))
        (testTime   "wed")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "32"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-23 12:00:00"))
        (testTime   "wenns")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "33"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-17 12:00:00"))
        (testTime   "thu")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "34"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-17 12:00:00"))
        (testTime   "thur")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "35"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-18 12:00:00"))
        (testTime   "fri")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "36"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-18 12:00:00"))
        (testTime   "fry")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "37"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-19 12:00:00"))
        (testTime   "sat")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "38"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-19 12:00:00"))
        (testTime   "satterday")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "39"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-20 12:00:00"))
        (testTime   "sun")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "40"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-20 12:00:00"))
        (testTime   "sum")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "41"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 09:00:00"))
        (testTime   "this morning")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "42"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 22:00:00"))
        (testTime   "tonight")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "43"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 15:30:00"))
        (testTime   "next hr")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "44"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 15:30:00"))
        (testTime   "next hrs")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "45"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 14:01:30"))
        (testTime   "next min")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "46"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 14:01:30"))
        (testTime   "next mins")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "47"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 14:01:30"))
        (testTime   "next minute")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "48"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 14:00:01"))
        (testTime   "next sec")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "49"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 14:00:01"))
        (testTime   "next secs")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "50"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 14:00:00"))
        (testTime   "this second")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "51"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 14:00:00"))
        (testTime   "this second")
        (currentTime chronicNowTime)
        (parserOptions [context Past])

    , monadicComaprisonCase "52"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 14:00:01"))
        (testTime   "next second")
        (currentTime chronicNowTime)
        (parserOptions [])

    , monadicComaprisonCase "53"
        (actualTime (timeLiteral (fmt "%F %T") "2006-08-16 13:59:59"))
        (testTime   "lasst second")
        (currentTime chronicNowTime)
        (parserOptions [])

    ]


{- 

    # month name
    # fortnight
    # day portion


  def test_parse_guess_grr
    time = parse_now("yesterday at 4:00")
    assert_equal Time.local(2006, 8, 15, 16), time

    time = parse_now("today at 9:00")
    assert_equal Time.local(2006, 8, 16, 9), time

    time = parse_now("today at 2100")
    assert_equal Time.local(2006, 8, 16, 21), time

    time = parse_now("this day at 0900")
    assert_equal Time.local(2006, 8, 16, 9), time

    time = parse_now("tomorrow at 0900")
    assert_equal Time.local(2006, 8, 17, 9), time

    time = parse_now("yesterday at 4:00", :ambiguous_time_range => :none)
    assert_equal Time.local(2006, 8, 15, 4), time

    time = parse_now("last friday at 4:00")
    assert_equal Time.local(2006, 8, 11, 16), time

    time = parse_now("next wed 4:00")
    assert_equal Time.local(2006, 8, 23, 16), time

    time = parse_now("yesterday afternoon")
    assert_equal Time.local(2006, 8, 15, 15), time

    time = parse_now("last week tuesday")
    assert_equal Time.local(2006, 8, 8, 12), time

    time = parse_now("tonight at 7")
    assert_equal Time.local(2006, 8, 16, 19), time

    time = parse_now("tonight 7")
    assert_equal Time.local(2006, 8, 16, 19), time

    time = parse_now("7 tonight")
    assert_equal Time.local(2006, 8, 16, 19), time
  end

  def test_parse_guess_grrr
    time = parse_now("today at 6:00pm")
    assert_equal Time.local(2006, 8, 16, 18), time

    time = parse_now("today at 6:00am")
    assert_equal Time.local(2006, 8, 16, 6), time

    time = parse_now("this day 1800")
    assert_equal Time.local(2006, 8, 16, 18), time

    time = parse_now("yesterday at 4:00pm")
    assert_equal Time.local(2006, 8, 15, 16), time

    time = parse_now("tomorrow evening at 7")
    assert_equal Time.local(2006, 8, 17, 19), time

    time = parse_now("tomorrow morning at 5:30")
    assert_equal Time.local(2006, 8, 17, 5, 30), time

    time = parse_now("next monday at 12:01 am")
    assert_equal Time.local(2006, 8, 21, 00, 1), time

    time = parse_now("next monday at 12:01 pm")
    assert_equal Time.local(2006, 8, 21, 12, 1), time

    # with context
    time = parse_now("sunday at 8:15pm", :context => :past)
    assert_equal Time.local(2006, 8, 13, 20, 15), time
  end

  def test_parse_guess_rgr
    time = parse_now("afternoon yesterday")
    assert_equal Time.local(2006, 8, 15, 15), time

    time = parse_now("tuesday last week")
    assert_equal Time.local(2006, 8, 8, 12), time
  end

  def test_parse_guess_a_ago
    time = parse_now("AN hour ago")
    assert_equal Time.local(2006, 8, 16, 13), time

    time = parse_now("A day ago")
    assert_equal Time.local(2006, 8, 15, 14), time

    time = parse_now("a month ago")
    assert_equal Time.local(2006, 7, 16, 14), time

    time = parse_now("a year ago")
    assert_equal Time.local(2005, 8, 16, 14), time
  end

  def test_parse_guess_s_r_p
    # past

    time = parse_now("3 years ago")
    assert_equal Time.local(2003, 8, 16, 14), time

    time = parse_now("1 month ago")
    assert_equal Time.local(2006, 7, 16, 14), time

    time = parse_now("1 fortnight ago")
    assert_equal Time.local(2006, 8, 2, 14), time

    time = parse_now("2 fortnights ago")
    assert_equal Time.local(2006, 7, 19, 14), time

    time = parse_now("3 weeks ago")
    assert_equal Time.local(2006, 7, 26, 14), time

    time = parse_now("2 weekends ago")
    assert_equal Time.local(2006, 8, 5), time

    time = parse_now("3 days ago")
    assert_equal Time.local(2006, 8, 13, 14), time

    #time = parse_now("1 monday ago")
    #assert_equal Time.local(2006, 8, 14, 12), time

    time = parse_now("5 mornings ago")
    assert_equal Time.local(2006, 8, 12, 9), time

    time = parse_now("7 hours ago")
    assert_equal Time.local(2006, 8, 16, 7), time

    time = parse_now("3 minutes ago")
    assert_equal Time.local(2006, 8, 16, 13, 57), time

    time = parse_now("20 seconds before now")
    assert_equal Time.local(2006, 8, 16, 13, 59, 40), time

    # future

    time = parse_now("3 years from now")
    assert_equal Time.local(2009, 8, 16, 14, 0, 0), time

    time = parse_now("6 months hence")
    assert_equal Time.local(2007, 2, 16, 14), time

    time = parse_now("3 fortnights hence")
    assert_equal Time.local(2006, 9, 27, 14), time

    time = parse_now("1 week from now")
    assert_equal Time.local(2006, 8, 23, 14, 0, 0), time

    time = parse_now("1 weekend from now")
    assert_equal Time.local(2006, 8, 19), time

    time = parse_now("2 weekends from now")
    assert_equal Time.local(2006, 8, 26), time

    time = parse_now("1 day hence")
    assert_equal Time.local(2006, 8, 17, 14), time

    time = parse_now("5 mornings hence")
    assert_equal Time.local(2006, 8, 21, 9), time

    time = parse_now("1 hour from now")
    assert_equal Time.local(2006, 8, 16, 15), time

    time = parse_now("20 minutes hence")
    assert_equal Time.local(2006, 8, 16, 14, 20), time

    time = parse_now("20 seconds from now")
    assert_equal Time.local(2006, 8, 16, 14, 0, 20), time

    time = Chronic.parse("2 months ago", :now => Time.parse("2007-03-07 23:30"))
    assert_equal Time.local(2007, 1, 7, 23, 30), time

    # Two repeaters
    time = parse_now("25 minutes and 20 seconds from now")
    assert_equal Time.local(2006, 8, 16, 14, 25, 20), time

    time = parse_now("24 hours and 20 minutes from now")
    assert_equal Time.local(2006, 8, 17, 14, 20, 0), time

    time = parse_now("24 hours 20 minutes from now")
    assert_equal Time.local(2006, 8, 17, 14, 20, 0), time
  end

  def test_parse_guess_p_s_r
    time = parse_now("in 3 hours")
    assert_equal Time.local(2006, 8, 16, 17), time
  end

  def test_parse_guess_s_r_p_a
    # past

    time = parse_now("3 years ago tomorrow")
    assert_equal Time.local(2003, 8, 17, 12), time

    time = parse_now("3 years ago this friday")
    assert_equal Time.local(2003, 8, 18, 12), time

    time = parse_now("3 months ago saturday at 5:00 pm")
    assert_equal Time.local(2006, 5, 19, 17), time

    time = parse_now("2 days from this second")
    assert_equal Time.local(2006, 8, 18, 14), time

    time = parse_now("7 hours before tomorrow at midnight")
    assert_equal Time.local(2006, 8, 17, 17), time

    # future
  end

  def test_parse_guess_o_r_g_r
    time = parse_now("3rd month next year", :guess => false)
    assert_equal Time.local(2007, 3), time.begin

    time = parse_now("3rd month next year", :guess => false)
    assert_equal Time.local(2007, 3, 1), time.begin

    time = parse_now("3rd thursday this september")
    assert_equal Time.local(2006, 9, 21, 12), time

    now = Time.parse("1/10/2010")
    time = parse_now("3rd thursday this november", :now => now)
    assert_equal Time.local(2010, 11, 18, 12), time

    time = parse_now("4th day last week")
    assert_equal Time.local(2006, 8, 9, 12), time
  end

  def test_parse_guess_nonsense
    time = parse_now("some stupid nonsense")
    assert_equal nil, time

    time = parse_now("Ham Sandwich")
    assert_equal nil, time

    time = parse_now("t")
    assert_equal nil, time
  end

  def test_parse_span
    span = parse_now("friday", :guess => false)
    assert_equal Time.local(2006, 8, 18), span.begin
    assert_equal Time.local(2006, 8, 19), span.end

    span = parse_now("november", :guess => false)
    assert_equal Time.local(2006, 11), span.begin
    assert_equal Time.local(2006, 12), span.end

    span = Chronic.parse("weekend" , :now => @time_2006_08_16_14_00_00, :guess => false)
    assert_equal Time.local(2006, 8, 19), span.begin
    assert_equal Time.local(2006, 8, 21), span.end
  end

  def test_parse_with_endian_precedence
    date = '11/02/2007'

    expect_for_middle_endian = Time.local(2007, 11, 2, 12)
    expect_for_little_endian = Time.local(2007, 2, 11, 12)

    # default precedence should be toward middle endianness
    assert_equal expect_for_middle_endian, Chronic.parse(date)

    assert_equal expect_for_middle_endian, Chronic.parse(date, :endian_precedence => [:middle, :little])

    assert_equal expect_for_little_endian, Chronic.parse(date, :endian_precedence => [:little, :middle])
  end

  def test_parse_words
    assert_equal parse_now("33 days from now"), parse_now("thirty-three days from now")
    assert_equal parse_now("2867532 seconds from now"), parse_now("two million eight hundred and sixty seven thousand five hundred and thirty two seconds from now")
    assert_equal parse_now("may 10th"), parse_now("may tenth")
    assert_equal parse_now("second monday in january"), parse_now("2nd monday in january")
  end

  def test_relative_to_an_hour_before
    # example prenormalization "10 to 2" becomes "10 minutes past 2"
    assert_equal Time.local(2006, 8, 16, 13, 50), parse_now("10 to 2")
    assert_equal Time.local(2006, 8, 16, 13, 50), parse_now("10 till 2")
    assert_equal Time.local(2006, 8, 16, 13, 50), parse_now("10 prior to 2")
    assert_equal Time.local(2006, 8, 16, 13, 50), parse_now("10 before 2")

    # uses the current hour, so 2006-08-16 13:50:00, not 14:50
    assert_equal Time.local(2006, 8, 16, 13, 50), parse_now("10 to")
    assert_equal Time.local(2006, 8, 16, 13, 50), parse_now("10 till")

    assert_equal Time.local(2006, 8, 16, 15, 45), parse_now("quarter to 4")
  end

  def test_relative_to_an_hour_after
    # not nil
    assert_equal Time.local(2006, 8, 16, 14, 10), parse_now("10 after 2")
    assert_equal Time.local(2006, 8, 16, 14, 10), parse_now("10 past 2")
    assert_equal Time.local(2006, 8, 16, 14, 30), parse_now("half past 2")
  end

  def test_parse_only_complete_pointers
    assert_equal parse_now("eat pasty buns today at 2pm"), @time_2006_08_16_14_00_00
    assert_equal parse_now("futuristically speaking today at 2pm"), @time_2006_08_16_14_00_00
    assert_equal parse_now("meeting today at 2pm"), @time_2006_08_16_14_00_00
  end

  def test_am_pm
    assert_equal Time.local(2006, 8, 16), parse_now("8/16/2006 at 12am")
    assert_equal Time.local(2006, 8, 16, 12), parse_now("8/16/2006 at 12pm")
  end

  def test_a_p
    assert_equal Time.local(2006, 8, 16, 0, 15), parse_now("8/16/2006 at 12:15a")
    assert_equal Time.local(2006, 8, 16, 18, 30), parse_now("8/16/2006 at 6:30p")
  end

  def test_seasons
    t = parse_now("this spring", :guess => false)
    assert_equal Time.local(2007, 3, 20), t.begin
    assert_equal Time.local(2007, 6, 20), t.end

    t = parse_now("this winter", :guess => false)
    assert_equal Time.local(2006, 12, 22), t.begin
    assert_equal Time.local(2007, 3, 19), t.end

    t = parse_now("last spring", :guess => false)
    assert_equal Time.local(2006, 3, 20), t.begin
    assert_equal Time.local(2006, 6, 20), t.end

    t = parse_now("last winter", :guess => false)
    assert_equal Time.local(2005, 12, 22), t.begin
    assert_equal Time.local(2006, 3, 19), t.end

    t = parse_now("next spring", :guess => false)
    assert_equal Time.local(2007, 3, 20), t.begin
    assert_equal Time.local(2007, 6, 20), t.end
  end

  # regression

  # def test_partial
  #   assert_equal '', parse_now("2 hours")
  # end

  def test_days_in_november
    t1 = Chronic.parse('1st thursday in november', :now => Time.local(2007))
    assert_equal Time.local(2007, 11, 1, 12), t1

    t1 = Chronic.parse('1st friday in november', :now => Time.local(2007))
    assert_equal Time.local(2007, 11, 2, 12), t1

    t1 = Chronic.parse('1st saturday in november', :now => Time.local(2007))
    assert_equal Time.local(2007, 11, 3, 12), t1

    # t1 = Chronic.parse('1st sunday in november', :now => Time.local(2007))
    # assert_equal Time.local(2007, 11, 4, 12), t1

    # Chronic.debug = true
    #
    # t1 = Chronic.parse('1st monday in november', :now => Time.local(2007))
    # assert_equal Time.local(2007, 11, 5, 11), t1
  end

  def test_now_changes
    t1 = Chronic.parse("now")
    sleep 0.1
    t2 = Chronic.parse("now")
    refute_equal t1, t2
  end

  def test_noon
    t1 = Chronic.parse('2011-01-01 at noon', :ambiguous_time_range => :none)
    assert_equal Time.local(2011, 1, 1, 12, 0), t1
  end

  def test_handle_rdn_rmn_sd
    time = parse_now("Thu Aug 10")
    assert_equal Time.local(2006, 8, 10, 12), time

    time = parse_now("Thursday July 31")
    assert_equal Time.local(2006, 7, 31, 12), time

    time = parse_now("Thursday December 31")
    assert_equal Time.local(2006, 12, 31, 12), time
  end

  def test_handle_rdn_rmn_sd_rt
    time = parse_now("Thu Aug 10 4pm")
    assert_equal Time.local(2006, 8, 10, 16), time

    time = parse_now("Thu Aug 10 at 4pm")
    assert_equal Time.local(2006, 8, 10, 16), time
  end

  def test_handle_rdn_rmn_od_rt
    time = parse_now("Thu Aug 10th at 4pm")
    assert_equal Time.local(2006, 8, 10, 16), time
  end

  def test_handle_rdn_od_rt
    time = parse_now("Thu 17th at 4pm")
    assert_equal Time.local(2006, 8, 17, 16), time

    time = parse_now("Thu 16th at 4pm")
    assert_equal Time.local(2006, 8, 16, 16), time

    time = parse_now("Thu 1st at 4pm")
    assert_equal Time.local(2006, 9, 1, 16), time

    time = parse_now("Thu 1st at 4pm", :context => :past)
    assert_equal Time.local(2006, 8, 1, 16), time
  end

  def test_handle_rdn_od
    time = parse_now("Thu 17th")
    assert_equal Time.local(2006, 8, 17, 12), time
  end

  def test_handle_rdn_rmn_sd_sy
    time = parse_now("Thu Aug 10 2006")
    assert_equal Time.local(2006, 8, 10, 12), time

    time = parse_now("Thursday July 31 2006")
    assert_equal Time.local(2006, 7, 31, 12), time

    time = parse_now("Thursday December 31 2006")
    assert_equal Time.local(2006, 12, 31, 12), time

    time = parse_now("Thursday December 30 2006")
    assert_equal Time.local(2006, 12, 30, 12), time
  end

  def test_handle_rdn_rmn_od
    time = parse_now("Thu Aug 10th")
    assert_equal Time.local(2006, 8, 10, 12), time

    time = parse_now("Thursday July 31st")
    assert_equal Time.local(2006, 7, 31, 12), time

    time = parse_now("Thursday December 31st")
    assert_equal Time.local(2006, 12, 31, 12), time
  end

  def test_handle_rdn_rmn_od_sy
    time = parse_now("Thu Aug 10th 2005")
    assert_equal Time.local(2005, 8, 10, 12), time

    time = parse_now("Thursday July 31st 2005")
    assert_equal Time.local(2005, 7, 31, 12), time

    time = parse_now("Thursday December 31st 2005")
    assert_equal Time.local(2005, 12, 31, 12), time

    time = parse_now("Thursday December 30th 2005")
    assert_equal Time.local(2005, 12, 30, 12), time
  end

  def test_normalizing_day_portions
    assert_equal pre_normalize("8:00 pm February 11"), pre_normalize("8:00 p.m. February 11")
  end

  private
  def parse_now(string, options={})
    Chronic.parse(string, {:now => TIME_2006_08_16_14_00_00 }.merge(options))
  end
  def pre_normalize(s)
    Chronic::Parser.new.pre_normalize s
  end
end
-}
