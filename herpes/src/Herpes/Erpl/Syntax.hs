module Herpes.Erpl.Syntax
  ( Module (..)
  , FormElement (..)
  , Statement (..)
  , Expression (..)
  ) where

import Data.Text (Text)
import Herpes.Identifier (Identifier)

data Module a
  = UseCaseModule a Identifier [FormElement a] [Statement a]
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data FormElement a
  = TextField a Identifier
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data Statement a
  = ExecuteSqlStatement a Text [Expression a]
  | MoveStatement a Identifier (Expression a)
  | WriteStatement a [Expression a]
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data Expression a
  = VariableExpression a Identifier
  deriving stock (Eq, Show, Functor, Foldable, Traversable)
