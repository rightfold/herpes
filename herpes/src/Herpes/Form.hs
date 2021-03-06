module Herpes.Form
  ( -- * Simple forms
    Form
  , Field (..)
  , labelField
  , textField

    -- * Composite forms
  , labeledForm

    -- * Simple wizards
  , Wizard
  ) where

import Control.Applicative.Free (Ap, liftAp)
import Control.Monad.Free (Free)
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

labeledForm :: Text -> Form a -> Form a
labeledForm l f = labelField l *> f

--------------------------------------------------------------------------------
-- Simple wizards

type Wizard = Free Form
