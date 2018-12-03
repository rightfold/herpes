module Herpes.Erpl.Syntax
  ( Identifier (..)
  , Module (..)
  , FormElement (..)
  , Statement (..)
  , Expression (..)
  ) where

import Data.Text (Text)

newtype Identifier =
  Identifier Text
  deriving stock (Eq, Show)

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
