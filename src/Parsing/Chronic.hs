module Parsing.Chronic
  ( MonadChronic(..)
   -- , 
   -- , 
    )
  where

import Text.ParserCombinators.Parsec.Char 
import Text.Parsec.Prim 
import qualified Data.Time as DT
import Text.ParserCombinators.Parsec.Combinator

class (Monad m) => MonadChronic m where
  getCurrentTime :: m DT.UTCTime

instance MonadChronic IO where
  getCurrentTime = DT.getCurrentTime
