module Herpes.UseCase
  ( UseCase (..)
  ) where

import Herpes.Screen (Screen)

data UseCase f =
  UseCase
    { useCaseScreen :: Screen f }
