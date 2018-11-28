module Herpes.Motd
  ( motdScreen
  , motdQuery
  , motdAction

  , example
  ) where

import Herpes.Form (Form)
import Herpes.Report (Coordinate (..), Report, Value (..))
import Herpes.Screen (Screen (..))
import Herpes.Screen.Gtk (renderScreen)

import qualified Graphics.UI.Gtk as Gtk
import qualified Herpes.Report as Report

motdScreen :: Screen IO
motdScreen = Screen motdQuery motdAction

motdQuery :: Form ()
motdQuery = pure ()

motdAction :: () -> IO (Report Value)
motdAction () = pure dummy
  where
  dummy :: Report Value
  dummy = foldr ($) Report.empty
    [ Report.insert (Coordinate 0 y) (TextValue "DATE") .
      Report.insert (Coordinate 1 y) (TextValue "MESSAGE")
    | y <- [0 .. 9] ]

example :: IO ()
example = do
  _ <- Gtk.initGUI
  window <- Gtk.windowNew
  Gtk.containerAdd window =<< renderScreen motdScreen
  Gtk.widgetShowAll window
  Gtk.mainGUI
