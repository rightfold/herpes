module Herpes.Report
  ( -- * Reports
    Report
  , Coordinate (..)
  , empty
  , insert
  , lookup
  , explore

    -- * State
  , HasReport (..)
  , HasReport'
  , put

    -- * Values
  , Value (..)
  ) where

import Prelude hiding (lookup)

import Control.Lens (Lens, (%=))
import Control.Monad.State.Class (MonadState)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict as HashMap

--------------------------------------------------------------------------------
-- Reports

data Report a =
  Report
    { reportValues :: HashMap Coordinate a
    , reportWidth  :: {-# UNPACK #-} Word
    , reportHeight :: {-# UNPACK #-} Word }
  deriving stock (Eq, Functor, Foldable, Traversable)

data Coordinate =
  Coordinate {-# UNPACK #-} Word
             {-# UNPACK #-} Word
  deriving stock (Eq, Ord, Read, Show, Generic)
  deriving anyclass (Hashable)

empty :: Report a
empty = Report HashMap.empty 0 0

insert :: Coordinate -> a -> Report a -> Report a
insert c@(Coordinate x y) v r =
  Report
    { reportValues = HashMap.insert c v (reportValues r)
    , reportWidth  = succ x `max` reportWidth r
    , reportHeight = succ y `max` reportHeight r }

lookup :: Coordinate -> Report a -> Maybe a
lookup = (. reportValues) . HashMap.lookup

explore :: Report a -> [[Maybe a]]
explore r =
  flip fmap [1 .. reportHeight r] $ \y ->
  flip fmap [1 .. reportWidth  r] $ \x ->
  HashMap.lookup (Coordinate (pred x) (pred y)) (reportValues r)

--------------------------------------------------------------------------------
-- State

class HasReport s t a b | s t -> a b where
  report :: Lens s t (Report a) (Report b)

instance HasReport (Report a) (Report b) a b where
  report = id

type HasReport' s a =
  HasReport s s a a

put :: (MonadState s f, HasReport' s a) => Coordinate -> a -> f ()
put = ((report %=) .) . insert

--------------------------------------------------------------------------------
-- Values

data Value
  = TextValue Text
  deriving stock (Eq, Ord, Read, Show)
