module Herpes.Form
  ( Form
  , Field (..)
  ) where

import Control.Applicative.Free (Ap)
import Data.Functor.Coyoneda (Coyoneda)
import Data.Text (Text)

type Form = Ap (Coyoneda Field)

data Field :: * -> * where
  LabelField :: Text -> Field ()
  TextField :: Field Text
