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
import qualified Control.Monad.State as CMS

class (Monad m) => MonadChronic m where
  getCurrentTime :: m DT.UTCTime

instance MonadChronic IO where
  getCurrentTime = DT.getCurrentTime


data Context = Future | Past deriving (Show, Eq)
data WeekStart = Sunday | Monday deriving (Show, Eq)
data EndianPrecedence = Middle | Little deriving (Show, Eq)

data Options = Options
  { _hours24                 :: Bool
  , _weekStart               :: WeekStart
  , _guess                   :: Bool
  , _ambiguousTimeRange      :: Maybe Int
  , _endianPrecedence        :: EndianPrecedence
  , _context                 :: Context
  , _ambiguousYearFutureBias :: Int
  } deriving (Show, Eq)

class ChronicOptions a where
  hours24            :: Bool -> a -> a
  weekStart          :: WeekStart -> a -> a
  guess              :: Bool -> a -> a
  ambiguousTimeRange :: Maybe Int -> a -> a
  endianPrecedence   :: EndianPrecedence -> a -> a

class ChronicMonadicOptions a where
  context                 :: Context -> a -> a
  ambiguousYearFutureBias :: Int -> a -> a

instance ChronicOptions Options where
  hours24 x o   = o {_hours24 = x}
  weekStart x o = o {_weekStart = x}
  guess x o     = o {_guess = x}
  ambiguousTimeRange x o = o {_ambiguousTimeRange = x}
  endianPrecedence x o = o {_endianPrecedence = x}

instance ChronicMonadicOptions Options where
  context x o = o {_context = x }
  ambiguousYearFutureBias x o = o {_ambiguousYearFutureBias = x}
