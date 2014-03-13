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
import qualified Data.Time as DT
import Text.ParserCombinators.Parsec.Combinator

f :: DT.UTCTime
f = undefined

g :: Bool
g = False

class (Monad m) => MonadChronic m where
  getCurrentTime :: m DT.UTCTime

instance MonadChronic IO where
  getCurrentTime = DT.getCurrentTime

--aTime = fromJust $ parseISO8601 "2011-10-05T14:48:00.000Z"
