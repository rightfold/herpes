module Herpes.Workspace
  ( Workspace
  ) where

import Data.HashMap.Strict (HashMap)
import Herpes.Identifier (Identifier)
import Herpes.UseCase (UseCase)

type Workspace f = HashMap Identifier (UseCase f)
