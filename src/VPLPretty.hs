module VPLPretty where

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ
import VPLTypes

renderFunDecl :: FunDecl -> String
renderFunDecl f = renderStyle s (prettyFunDecl f)
  where
    s = Style {mode = PageMode, lineLength = 100, ribbonsPerLine = 1.5}

prettyFunDecl :: FunDecl -> Doc
prettyFunDecl (FunDecl name args body) =
  text name <+> fsep (map text args) <+> char '=' <+> prettyBody body
  -- where
  --   i = length name

prettyBody :: Body -> Doc
prettyBody b =
  brackets (fcat (punctuate (char ',' <> space) (map prettyStmt b)))

prettyStmt :: Stmt -> Doc
prettyStmt (TurnLeft e) = text "↺" <+> prettyExpr e
prettyStmt (TurnRight e) = text "↻" <+> prettyExpr e
prettyStmt (Forward e) = text "↑" <+> prettyExpr e
prettyStmt PenUp = text "○"
prettyStmt PenDown = text "●"
prettyStmt (FunCall name es) = text name <+> fsep (map prettyExpr es)
prettyStmt (Loop e b) = text "loop" <+> prettyExpr e <+> prettyBody b

prettyExpr :: Expr -> Doc
prettyExpr (Var v) = text v
prettyExpr (Lit f) = int (floor f)
