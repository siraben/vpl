module VPLPretty where

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ
import VPLTypes

renderFunDecl :: FunDecl -> String
renderFunDecl f = renderStyle defaultStyle (prettyFunDecl f)

render = renderStyle defaultStyle

defaultStyle = Style {mode = PageMode, lineLength = 100, ribbonsPerLine = 1.5}

prettyFunDecl :: FunDecl -> Doc
prettyFunDecl (FunDecl name args body) =
  text name <+> fsep (map text args) <+> char '=' <+> prettyBody body

prettyBody :: Body -> Doc
prettyBody b =
  brackets (fcat (punctuate (char ',' <> space) (map prettyStmt b)))

prettyStmt :: Stmt -> Doc
prettyStmt (TurnLeft e) = text "↺" <+> prettyExpr e
prettyStmt (TurnRight e) = text "↻" <+> prettyExpr e
prettyStmt (Forward e) = text "↑" <+> prettyExpr e
prettyStmt PenUp = text "○"
prettyStmt PenDown = text "●"
prettyStmt (FunCall name es) =
  text name <+>
  (if es == []
     then mempty
     else fsep (map (\x -> text "(" <> prettyExpr x <> text ")") es))
prettyStmt (Loop e b) = text "loop" <+> prettyExpr e <+> prettyBody b
prettyStmt (If p c a) =
  fsep
    [ text "if"
    , prettyBExpr p
    , text "then"
    , prettyBody c
    , text "else"
    , prettyBody a
    ]

prettyExpr :: Expr -> Doc
prettyExpr (Var v) = text v
prettyExpr (Lit f) = int (floor f)
prettyExpr (Add a b) = fsep [prettyExpr a, text "+", prettyExpr b]
prettyExpr (Mul a b) = fsep [prettyExpr a, text "*", prettyExpr b]
prettyExpr (Div a b) = fsep [prettyExpr a, text "/", prettyExpr b]
prettyExpr (Sub a b) = fsep [prettyExpr a, text "-", prettyExpr b]

prettyBExpr :: BExpr -> Doc
prettyBExpr (IsZero e) = fsep [text "zero?", prettyExpr e]
