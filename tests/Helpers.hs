module Helpers
  ( ChronicTest
  , MonadChronic
  , runChronicTest
  , timeLiteral
  ) where


newtype ChronicTest a = ChronicTest { getChronicTest :: CMS.State DT.UTCTime a }

instance Monad ChronicTest where
  return = ChronicTest . return
  c >>= f = ChronicTest $ getChronicTest c >>= getChronicTest . f

instance MonadChronic ChronicTest where
  getCurrentTime = ChronicTest get


runChronicTest :: DT.UTCTime -> ChronicTest a -> a
runChronicTest t ct = fst $ CMS.runState (getChronicTest ct) t

timeLiteral = fromJust . parseISO8601
