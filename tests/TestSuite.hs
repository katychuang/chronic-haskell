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



simpleComparisonCase name format time = testCase name $
  Right (timeLiteral format time) @=? parserUnderTest time

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
    ]

