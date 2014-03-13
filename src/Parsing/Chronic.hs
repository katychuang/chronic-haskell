module Parsing.Chronic
{-
    ( _areaCode
    , f
    , g
    )
-}
    where

import Text.ParserCombinators.Parsec.Char 
import Text.Parsec.Prim 
import qualified Control.Monad.State as CMS
import qualified Data.Time as DT
import Control.Monad.State.Class
import Text.ParserCombinators.Parsec.Combinator
import Data.Time.ISO8601
import Data.Maybe

f :: Bool
f = False

g :: Bool
g = False


class (Monad m) => MonadChronic m where
  getCurrentTime :: m DT.UTCTime

instance MonadChronic IO where
  getCurrentTime = DT.getCurrentTime

newtype ChronicTest a = ChronicTest { getChronicTest :: CMS.State DT.UTCTime a }

instance Monad ChronicTest where
  return = ChronicTest . return
  c >>= f = ChronicTest $ getChronicTest c >>= getChronicTest . f

instance MonadChronic ChronicTest where
  getCurrentTime = ChronicTest get


runChronicTest :: DT.UTCTime -> ChronicTest a -> a
runChronicTest t ct = fst $ CMS.runState (getChronicTest ct) t

now :: (MonadChronic m) => m DT.UTCTime
now = getCurrentTime

aTime = fromJust $ parseISO8601 "2011-10-05T14:48:00.000Z"
