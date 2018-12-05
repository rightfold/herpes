module Herpes.Erpl.Translate
  ( translateModule
  , translateFormElement
  , translateStatement
  , translateExpression
  ) where

import Control.Lens ((^.), re, to)

import qualified Data.Text as Text
import qualified Herpes.Erpl.Syntax as E
import qualified Herpes.Identifier as E
import qualified Language.Haskell.Exts.Syntax as H

--------------------------------------------------------------------------------
-- Modules

translateModule :: E.Module () -> H.Module ()

translateModule (E.UseCaseModule () eName eFormElements eStatements) =
  let
    hHead =
      let
        hName = H.ModuleName () (eName ^. re E._Identifier . to Text.unpack)
        hWarningText = Nothing
        hExportList = Nothing
      in
        Just $ H.ModuleHead () hName hWarningText hExportList
    hPragmas = []
    hImports = []
    hDecls = []
  in
    H.Module () hHead hPragmas hImports hDecls

--------------------------------------------------------------------------------
-- Form elements

translateFormElement :: E.FormElement () -> [(E.Identifier, H.Exp ())]

translateFormElement (E.TextField () eName) =
  let initial = H.Var () Sn_Text_empty in
  let expr = H.App () (H.Var () Sn_Herpes_Form_textField) initial in
  [(eName, expr)]

--------------------------------------------------------------------------------
-- Statements

translateStatement :: E.Statement () -> H.Exp ()

translateStatement (E.ExecuteSqlStatement () query arguments) =
  undefined

translateStatement (E.MoveStatement () target source) =
  undefined

translateStatement (E.WriteStatement () arguments) =
  undefined

--------------------------------------------------------------------------------
-- Expressions

translateExpression :: E.Expression () -> H.Exp ()

translateExpression (E.VariableExpression () name) =
  undefined

--------------------------------------------------------------------------------
-- Special names

pattern Sn_Herpes_Form = H.ModuleName () "Herpes.Form"
pattern Sn_Text = H.ModuleName () "Data.Text"

pattern Sn_Herpes_Form_textField = H.Qual () Sn_Herpes_Form (H.Ident () "textField")
pattern Sn_Text_empty = H.Qual () Sn_Text (H.Ident () "empty")
