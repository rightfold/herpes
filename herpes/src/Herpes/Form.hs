module Herpes.Form
  ( -- * Simple forms
    Form
  , Field (..)
  , labelField
  , textField

    -- * Composite forms
  , labeledField
  ) where

import Control.Applicative.Free (Ap, liftAp)
import Data.Functor.Coyoneda (Coyoneda, liftCoyoneda)
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Simple forms

type Form = Ap (Coyoneda Field)

data Field :: * -> * where
  LabelField :: Text -> Field ()
  TextField :: Text -> Field Text

labelField :: Text -> Form ()
labelField = liftAp . liftCoyoneda . LabelField

textField :: Text -> Form Text
textField = liftAp . liftCoyoneda . TextField

--------------------------------------------------------------------------------
-- Composite forms

labeledField :: Text -> Form a -> Form a
labeledField l f = labelField l *> f
