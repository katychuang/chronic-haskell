{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import Parsing.Chronic
import Helpers

import Test.Framework (defaultMain)
import           Test.Framework                     (Test, testGroup)
import           Test.Framework.Providers.HUnit     (testCase)
import           Test.HUnit                         ((@=?))


main :: IO ()
main = defaultMain
    [ testHandleGeneric
    ]

testHandleGeneric :: Test
testHandleGeneric = testGroup "test_handle_generic"
    [ testCase "1" $
        (timeLiteral (fmt "%FT%T") "2012-08-02T13:00:00") @=? parserUnderTest "2012-08-02T13:00:00"

    , testCase "2" $
        False @=? g

    ]

