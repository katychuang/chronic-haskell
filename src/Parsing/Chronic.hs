module Parsing.Chronic
  ( MonadChronic(..)
  , ChronicOption(..)
  , ChronicMonadicOption(..)
  , Guess(..)
  , Context(..)
  , WeekStart(..)
  , EndianPrecedence(..)
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

data Options = Options
  { _hours24                 :: Bool
  , _weekStart               :: WeekStart
  , _guess                   :: Guess
  , _ambiguousTimeRange      :: Maybe Int
  , _endianPrecedence        :: EndianPrecedence
  , _context                 :: Context
  , _ambiguousYearFutureBias :: Int
  } deriving (Show, Eq)

class ChronicOption a where
  hours24            :: Bool -> a -> a
  weekStart          :: WeekStart -> a -> a
  guess              :: Guess -> a -> a
  ambiguousTimeRange :: Maybe Int -> a -> a
  endianPrecedence   :: EndianPrecedence -> a -> a

class ChronicMonadicOption a where
  context                 :: Context -> a -> a
  ambiguousYearFutureBias :: Int -> a -> a

instance ChronicOption Options where
  hours24 x o   = o {_hours24 = x}
  weekStart x o = o {_weekStart = x}
  guess x o     = o {_guess = x}
  ambiguousTimeRange x o = o {_ambiguousTimeRange = x}
  endianPrecedence x o = o {_endianPrecedence = x}

instance ChronicMonadicOption Options where
  context x o = o {_context = x }
  ambiguousYearFutureBias x o = o {_ambiguousYearFutureBias = x}
