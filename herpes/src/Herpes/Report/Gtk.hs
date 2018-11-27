module Herpes.Report.Gtk
  ( renderReport

  , example
  ) where

import Data.Foldable (for_)
import Data.Function ((&))
import Data.Text (Text)
import Graphics.UI.Gtk (AttrOp ((:=)))
import Herpes.Report (Coordinate (..), Report, Value (..))

import qualified Graphics.UI.Gtk as Gtk
import qualified Herpes.Report as Report

renderReport :: Report Value -> IO Gtk.Widget
renderReport report = do
  treeModel <- Gtk.listStoreNew (Report.explore report)

  treeView <- Gtk.treeViewNewWithModel treeModel
  for_ [1 .. Report.width report] $ \(pred -> x) -> do
    column <- makeColumn treeModel x
    Gtk.treeViewAppendColumn treeView column

  pure (Gtk.toWidget treeView)

  where

  makeColumn :: Gtk.ListStore [Maybe Value] -> Word -> IO Gtk.TreeViewColumn
  makeColumn treeModel (fromIntegral -> i) = do
    column <- Gtk.treeViewColumnNew
    cellRenderer <- Gtk.cellRendererTextNew
    Gtk.treeViewColumnPackStart column cellRenderer False
    Gtk.cellLayoutSetAttributes column cellRenderer treeModel $
      \row -> [Gtk.cellText := cellText (row !! i)]
    pure column

  cellText :: Maybe Value -> Text
  cellText Nothing = ""
  cellText (Just (TextValue t)) = t

example :: IO ()
example = do
  _ <- Gtk.initGUI

  widget <- renderReport $
    Report.empty
    & Report.insert (Coordinate 0 0) (TextValue "HELLO")
    & Report.insert (Coordinate 1 0) (TextValue "WORLD")
    & Report.insert (Coordinate 0 1) (TextValue "KEK")
    & Report.insert (Coordinate 1 1) (TextValue "LOL")

  window <- Gtk.windowNew
  Gtk.containerAdd window widget
  Gtk.widgetShowAll window

  Gtk.mainGUI

