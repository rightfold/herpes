module Herpes.Workspace
  ( Workspace
  ) where

import Data.HashMap.Strict (HashMap)
import Herpes.UseCase (UseCase, UseCaseId)

type Workspace f = HashMap UseCaseId (UseCase f)
