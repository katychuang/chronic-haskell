module Helpers
  ( ChronicTest
  , MonadChronic
  , runChronicTest
  , timeLiteral
  , fmt
  , parserUnderTest
  , parserUnderTestMOpts
  ) where

import qualified Control.Monad.State as CMS
import Data.Maybe (fromJust)
import qualified Data.Time as DT
import System.Locale (defaultTimeLocale)
import Parsing.Chronic

newtype ChronicTest a = ChronicTest { getChronicTest :: CMS.State DT.UTCTime a }

instance Monad ChronicTest where
  return = ChronicTest . return
  c >>= f = ChronicTest $ getChronicTest c >>= getChronicTest . f

instance MonadChronic ChronicTest where
  getCurrentTime = ChronicTest CMS.get

runChronicTest :: DT.UTCTime -> ChronicTest a -> a
runChronicTest t ct = fst $ CMS.runState (getChronicTest ct) t

timeLiteral :: Format -> String -> DT.UTCTime
timeLiteral f s = case(DT.parseTime defaultTimeLocale (getFormat f) s) of
  Just x -> x
  Nothing -> error ("could not parse for format string: " ++ (getFormat f) ++ " and input: " ++ s)

newtype Format = Format { getFormat :: String }

fmt = Format

parserUnderTest :: String -> Either String DT.UTCTime
parserUnderTest x = Right $ timeLiteral (fmt "%FT%T") "2012-08-02T13:00:00"

parserUnderTestOpts :: [(ChronicOptions -> ChronicOptions)] -> String -> Either String DT.UTCTime
parserUnderTestOpts o x = Right $ timeLiteral (fmt "%FT%T") "2012-08-02T13:00:00"

parserUnderTestM :: (MonadChronic m) => String -> m (Either String DT.UTCTime)
parserUnderTestM x = return $ Right $ timeLiteral (fmt "%FT%T") "2012-08-02T13:00:00"

parserUnderTestMOpts :: (MonadChronic m)
                     => [(ChronicOptions -> ChronicOptions)] -> String -> m (Either String DT.UTCTime)
parserUnderTestMOpts o x = return $ Right $ timeLiteral (fmt "%FT%T") "2012-08-02T13:00:00"
