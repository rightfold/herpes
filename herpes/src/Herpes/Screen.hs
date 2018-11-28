module Herpes.Screen
  ( Screen (..)
  ) where

import Herpes.Form (Form)
import Herpes.Report (Report, Value)

data Screen f where
  Screen :: { screenQuery  :: Form a
            , screenAction :: a -> f (Report Value) }
         -> Screen f
