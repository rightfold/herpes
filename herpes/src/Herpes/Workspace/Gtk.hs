module Herpes.Workspace.Gtk
  ( renderWorkspace

  , example
  ) where

import Control.Lens ((^?), (^?!), ix)
import Data.Text (Text)
import Data.Thyme.Calendar (Day (..))
import Data.Thyme.LocalTime (LocalTime (..), ZonedTime (..), midnight, utc)
import Herpes.Identifier (Identifier, _Identifier)
import Herpes.Screen.Gtk (renderScreen)
import Herpes.UseCase (UseCase (..))
import Herpes.Workspace (Workspace)

import qualified Data.HashMap.Strict as HashMap
import qualified Graphics.UI.Gtk as Gtk
import qualified Herpes.Motd as Motd

renderWorkspace :: Workspace IO -> IO Gtk.Widget
renderWorkspace workspace = do
  notebook <- makeNotebook
  toolbar <- makeToolbar $ \useCaseId ->
               case workspace ^? ix useCaseId of
                 Nothing -> displayError "Unknown use case"
                 Just useCase -> instantiateUseCase notebook useCase

  box <- Gtk.vBoxNew False 0
  Gtk.boxPackStart box toolbar Gtk.PackNatural 0
  Gtk.boxPackStart box notebook Gtk.PackGrow 0

  pure (Gtk.toWidget box)

makeNotebook :: IO Gtk.Container
makeNotebook = do
  notebook <- Gtk.notebookNew
  pure (Gtk.toContainer notebook)

makeToolbar :: (Identifier -> IO ()) -> IO Gtk.Widget
makeToolbar callback = do
  useCaseIdEntry <- Gtk.entryNew
  useCaseGoButton <- Gtk.buttonNewWithLabel @String "Go"

  _ <- Gtk.on useCaseGoButton Gtk.buttonActivated $ do
    rawUseCaseId <- Gtk.entryGetText useCaseIdEntry
    case rawUseCaseId ^? _Identifier of
      Nothing -> displayError "Invalid use case identifier"
      Just useCaseId -> callback useCaseId

  toolbar <- Gtk.toolbarNew
  Gtk.containerAdd toolbar useCaseIdEntry
  Gtk.containerAdd toolbar useCaseGoButton

  pure (Gtk.toWidget toolbar)

instantiateUseCase :: Gtk.Container -> UseCase IO -> IO ()
instantiateUseCase notebook (UseCase screen) = do
  widget <- renderScreen screen
  Gtk.containerAdd notebook widget
  Gtk.widgetShowAll widget

displayError :: Text -> IO ()
displayError msg = do
  dialog <- Gtk.messageDialogNew Nothing [] Gtk.MessageError Gtk.ButtonsOk msg
  _ <- Gtk.on dialog Gtk.response $ \_ -> Gtk.widgetDestroy dialog
  Gtk.widgetShowAll dialog

example :: IO ()
example = do
  _ <- Gtk.initGUI

  let time d = ZonedTime (LocalTime (ModifiedJulianDay d) midnight) utc
  let getMessages = pure [ Motd.Message (time 4) "Hello"
                         , Motd.Message (time 3) "Hallo"
                         , Motd.Message (time 2) "Hola"
                         , Motd.Message (time 1) "今日は" ]
  workspaceWidget <- renderWorkspace $
    HashMap.singleton ("MOTD" ^?! _Identifier) (UseCase (Motd.screen getMessages))

  window <- Gtk.windowNew
  Gtk.containerAdd window workspaceWidget
  Gtk.widgetShowAll window

  Gtk.mainGUI
