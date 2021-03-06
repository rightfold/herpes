module Herpes.Form.Gtk
  ( renderForm
  , renderField

  , example
  ) where

import Control.Applicative.Free (runAp)
import Data.Foldable (for_)
import Data.Functor.Coyoneda (Coyoneda (..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Herpes.Form (Field (..), Form)
import Herpes.Form.Generic (gformlet)

import qualified GHC.Generics as Ghc
import qualified Generics.SOP as Sop
import qualified Graphics.UI.Gtk as Gtk

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
  entry <- Gtk.entryNew
  Gtk.entrySetText entry value
  pure ([Gtk.toWidget entry], Gtk.entryGetText entry)

data User =
  User
    { userName :: Text
    , userAge  :: Text
    , userFood :: Text }
  deriving stock (Ghc.Generic, Show)
  deriving anyclass (Sop.Generic, Sop.HasDatatypeInfo)

example :: IO ()
example = do
  _ <- Gtk.initGUI

  (widgets, get) <- renderForm $
    gformlet User { userName = "Arian", userAge = "23", userFood = "rijst" }

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
