module Herpes.UseCase
  ( UseCaseId, _UseCaseId
  , UseCase (..)
  ) where

import Control.Lens (Prism', prism')
import Control.Monad (guard)
import Data.Hashable (Hashable)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Herpes.Screen (Screen)

import qualified Data.Text as Text

newtype UseCaseId =
  UseCaseId Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable)

_UseCaseId :: Prism' Text UseCaseId
_UseCaseId = prism' get set
  where get (UseCaseId a) = a
        set a = UseCaseId a <$ guard (all allowed (Text.unpack a))
        allowed = (`elem` ['A' .. 'Z'] <> ['0' .. '9'] <> "-")

data UseCase f =
  UseCase
    { useCaseScreen :: Screen f }
