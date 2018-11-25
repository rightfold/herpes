{-# LANGUAGE OverloadedStrings #-}

module Herpes.Form.Gtk
  ( renderForm
  , renderField

  , example
  ) where

import Control.Applicative.Free (runAp)
import Data.Foldable (for_)
import Data.Functor.Coyoneda (Coyoneda (..))
import Data.Semigroup ((<>))
import Herpes.Form (Field (..), Form, labeledForm, textField)

import qualified Graphics.UI.Gtk as Gtk
import qualified System.Glib.Attributes as Glib

newtype GtkForm a =
  GtkForm { runGtkForm :: IO ([Gtk.Widget], IO a) }
  deriving stock (Functor)

instance Applicative GtkForm where
  pure x = GtkForm $ pure ([], pure x)
  (<*>) (GtkForm a) (GtkForm b) = GtkForm $
    [ (aws <> bws, ar <*> br) | (aws, ar) <- a, (bws, br) <- b ]

renderForm :: Form a -> IO ([Gtk.Widget], IO a)
renderForm form =
  let
    step (Coyoneda result field) =
      result <$> GtkForm (renderField field)
  in
    runGtkForm (runAp step form)

renderField :: Field a -> IO ([Gtk.Widget], IO a)

renderField (LabelField text) = do
  label <- Gtk.labelNew (Just text)
  pure ([Gtk.toWidget label], pure ())

renderField (TextField value) = do
  textView <- Gtk.textViewNew
  textBuffer <- Glib.get textView Gtk.textViewBuffer
  Gtk.textBufferSetText textBuffer value
  let getValue = do
        startIter <- Gtk.textBufferGetStartIter textBuffer
        endIter   <- Gtk.textBufferGetEndIter   textBuffer
        Gtk.textBufferGetText textBuffer startIter endIter False
  pure ([Gtk.toWidget textView], getValue)

example :: IO ()
example = do
  _ <- Gtk.initGUI

  (widgets, get) <-
    renderForm $
      (,,) <$> labeledForm "Name" (textField "Arian")
           <*> labeledForm "Age"  (textField "23")
           <*> labeledForm "Food" (textField "rijst")

  vbox <- Gtk.toBox <$> Gtk.vBoxNew False 10
  for_ widgets $ \widget ->
    Gtk.boxPackStart vbox widget Gtk.PackNatural 0

  submit <- Gtk.buttonNewWithLabel @String "Submit"
  _ <- Gtk.on submit Gtk.buttonActivated $ print =<< get
  Gtk.boxPackStart vbox submit Gtk.PackNatural 0

  window <- Gtk.windowNew
  Gtk.containerAdd window vbox
  Gtk.widgetShowAll window

  Gtk.mainGUI
