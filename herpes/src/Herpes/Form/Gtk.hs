module Herpes.Form.Gtk
  ( renderForm
  , renderField
  ) where

import Control.Applicative.Free (runAp_)
import Data.Functor.Coyoneda (Coyoneda (..))
import Herpes.Form (Field (..), Form)

import qualified Graphics.UI.Gtk.Abstract.Widget as Gtk
import qualified Graphics.UI.Gtk.Display.Label as Gtk
import qualified Graphics.UI.Gtk.Multiline.TextBuffer as Gtk
import qualified Graphics.UI.Gtk.Multiline.TextView as Gtk
import qualified System.Glib.Attributes as Glib

renderForm :: Form a -> IO [Gtk.Widget]
renderForm =
  runAp_ $ \(Coyoneda _ field) ->
    renderField field

renderField :: Field a -> IO [Gtk.Widget]

renderField (LabelField text) = do
  label <- Gtk.labelNew (Just text)
  pure [Gtk.toWidget label]

renderField (TextField value) = do
  textView <- Gtk.textViewNew
  textBuffer <- Glib.get textView Gtk.textViewBuffer
  Gtk.textBufferSetText textBuffer value
  pure [Gtk.toWidget textView]
