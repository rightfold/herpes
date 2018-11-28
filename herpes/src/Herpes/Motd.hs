module Herpes.Motd
  ( -- * Screen
    motdScreen
  , motdQuery
  , motdAction

    -- * Data
  , Message (..)

    -- * Miscellaneous
  , example
  ) where

import Data.Text (Text)
import Data.Thyme.Calendar (Day (..))
import Data.Thyme.LocalTime (LocalTime (..), ZonedTime (..), midnight, utc)
import Herpes.Form (Form)
import Herpes.Report (Coordinate (..), Report, Value (..))
import Herpes.Screen (Screen (..))
import Herpes.Screen.Gtk (renderScreen)

import qualified Data.Text as Text
import qualified Graphics.UI.Gtk as Gtk
import qualified Herpes.Report as Report

--------------------------------------------------------------------------------
-- Screen

motdScreen :: Functor f => f [Message] -> Screen f
motdScreen = Screen motdQuery . motdAction

motdQuery :: Form ()
motdQuery = pure ()

motdAction :: Functor f => f [Message] -> () -> f (Report Value)
motdAction getMessages () = do
  messages <- getMessages
  pure $ foldr ($) Report.empty $
    [ Report.insert (Coordinate 0 y) (TextValue (Text.pack (show posted))) .
      Report.insert (Coordinate 1 y) (TextValue body)
    | Message posted body <- messages
    | y <- [0 ..] ]

--------------------------------------------------------------------------------
-- Data

data Message =
  Message
    { messagePosted :: ZonedTime
        -- ^
        -- The reader may be interested in the time of day the author posted
        -- the message, hence use a 'ZonedTime'.
    , messageBody :: Text }
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Miscellaneous

example :: IO ()
example = do
  _ <- Gtk.initGUI

  let time d = ZonedTime (LocalTime (ModifiedJulianDay d) midnight) utc
  let getMessages = pure [ Message (time 4) "Hello"
                         , Message (time 3) "Hallo"
                         , Message (time 2) "Hola"
                         , Message (time 1) "今日は" ]
  widget <- renderScreen (motdScreen getMessages)

  window <- Gtk.windowNew
  Gtk.containerAdd window widget
  Gtk.widgetShowAll window
  Gtk.mainGUI
