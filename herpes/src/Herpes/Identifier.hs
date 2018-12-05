module Herpes.Identifier
  ( Identifier
  , _Identifier
  ) where

import Control.Lens (Prism', prism')
import Control.Monad (guard)
import Data.Hashable (Hashable)
import Data.Semigroup ((<>))
import Data.Text (Text)

import qualified Data.Text as Text

newtype Identifier =
  Identifier Text
  deriving stock (Eq, Show)
  deriving newtype (Hashable)

_Identifier :: Prism' Text Identifier
_Identifier = prism' get set
  where get (Identifier a) = a
        set a = Identifier a <$ guard (all allowed (Text.unpack a))
        allowed = (`elem` ['A' .. 'Z'] <> ['0' .. '9'] <> "-")
