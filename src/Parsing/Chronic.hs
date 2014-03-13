module Parsing.Chronic
  ( MonadChronic(..)
  , ChronicOptions(..)
  , Guess(..)
  , Context(..)
  , WeekStart(..)
  , EndianPrecedence(..)
  , hours24
  , weekStart
  , guess
  , ambiguousTimeRange
  , endianPrecedence
  , context
  , ambiguousYearFutureBias
   -- , 
    )
  where

import Text.ParserCombinators.Parsec.Char 
import Text.Parsec.Prim 
import qualified Data.Time as DT
import Text.ParserCombinators.Parsec.Combinator
import qualified Control.Monad.State as CMS

class (Monad m) => MonadChronic m where
  getCurrentTime :: m DT.UTCTime

instance MonadChronic IO where
  getCurrentTime = DT.getCurrentTime


data Guess = Guess Bool | End | Middle | Begin deriving (Show, Eq)
data Context = Future | Past deriving (Show, Eq)
data WeekStart = Sunday | Monday deriving (Show, Eq)
data EndianPrecedence = MiddleEnd | Little deriving (Show, Eq)

data ChronicOptions = ChronicOptions
  { _hours24                 :: Bool
  , _weekStart               :: WeekStart
  , _guess                   :: Guess
  , _ambiguousTimeRange      :: Maybe Int
  , _endianPrecedence        :: EndianPrecedence
  -- monadic options
  , _context                 :: Context
  , _ambiguousYearFutureBias :: Int
  } deriving (Show, Eq)

hours24 x o   = o {_hours24 = x}
weekStart x o = o {_weekStart = x}
guess x o     = o {_guess = x}
ambiguousTimeRange x o = o {_ambiguousTimeRange = x}
endianPrecedence x o = o {_endianPrecedence = x}

context x o = o {_context = x }
ambiguousYearFutureBias x o = o {_ambiguousYearFutureBias = x}
