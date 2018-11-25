module Herpes.Form
  ( Form
  , Field (..)
  , labelField
  , textField
  ) where

import Control.Applicative.Free (Ap, liftAp)
import Data.Functor.Coyoneda (Coyoneda, liftCoyoneda)
import Data.Text (Text)

type Form = Ap (Coyoneda Field)

data Field :: * -> * where
  LabelField :: Text -> Field ()
  TextField :: Field Text

labelField :: Text -> Form ()
labelField = liftAp . liftCoyoneda . LabelField

textField :: Form Text
textField = liftAp . liftCoyoneda $ TextField
