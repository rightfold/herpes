module Herpes.Screen.Gtk
  ( renderScreen

  , example
  ) where

import Data.Foldable (for_)
import Data.Function ((&))
import Herpes.Form.Gtk (renderForm)
import Herpes.Report (Coordinate (..), Value (..))
import Herpes.Report.Gtk (renderReport)
import Herpes.Screen (Screen (..))

import qualified Graphics.UI.Gtk as Gtk
import qualified Herpes.Form as Form
import qualified Herpes.Report as Report

renderScreen :: Screen IO -> IO Gtk.Widget
renderScreen (Screen query action) = do
  paned <- Gtk.vPanedNew

  formBox   <- Gtk.vBoxNew False 10
  reportBox <- Gtk.vBoxNew False 10
  Gtk.panedAdd1 paned formBox
  Gtk.panedAdd2 paned reportBox

  (formWidgets, formGetValue) <- renderForm query
  for_ formWidgets $ \widget ->
    Gtk.containerAdd formBox widget
  submitButton <- Gtk.buttonNewWithLabel @String "Submit"
  Gtk.containerAdd formBox submitButton

  _ <- Gtk.on submitButton Gtk.buttonActivated $ do
    Gtk.containerForeach reportBox (Gtk.containerRemove reportBox)
    reportWidget <- renderReport =<< action =<< formGetValue
    Gtk.containerAdd reportBox reportWidget
    Gtk.widgetShowAll reportWidget

  pure (Gtk.toWidget paned)

example :: IO ()
example = do
  _ <- Gtk.initGUI

  widget <- renderScreen $
    let query = (,) <$> Form.textField "A" <*> Form.textField "B" in
    let action (a, b) =
          pure $ Report.empty &
                 Report.insert (Coordinate 0 0) (TextValue a) &
                 Report.insert (Coordinate 1 0) (TextValue b) in
    Screen { screenQuery  = query
           , screenAction = action }

  window <- Gtk.windowNew
  Gtk.containerAdd window widget
  Gtk.widgetShowAll window

  Gtk.mainGUI
