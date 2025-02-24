module PPAsl 
where

import Common
import AST
import Text.PrettyPrint
import Prelude hiding ((<>))

tabW :: Int
tabW = 2

pVars :: [Name] -> Doc
pVars [v] = pVar v 
pVars (v:vs) = pVar v <+> text "," <+> pVars vs 
pVars _ = undefined -- imposible

pVar :: Name -> Doc
pVar = text 

-- Fondos
pFill :: Fill -> Doc
pFill Full = text "lleno"
pFill Outline = text "contorno"

colorSquare :: Int -> Int -> Int -> Doc
colorSquare r g b = text $ "\ESC[48;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m  \ESC[0m"


pColor :: Exp -> Doc
pColor (Color r g b) = text "#" <+> int r <+> int g <+> int b <+> colorSquare r g b
pColor (Var n) = pVar n
pColor _ = undefined -- imposible


pType :: Type -> Doc
pType ColorT  = text "Color"
pType ImageT  = text "Image"
pType AnimT   = text "Anim"
pType ActionT = text "Action" 

pPoint :: Point -> Doc
pPoint (Point a b) = text "{" <+> float a <+> text ", " <+> float b <+> text "}"

-- Expresiones
pExp :: Exp -> Doc
-- Constructores de Imagenes
pExp (Circle i1 i2 f c)     = 
    text "circle" <+> float i1 <+> float i2 <+> pFill f <+> pColor c
pExp (Rect i1 i2 i3 f c)    = 
    text "rect" <+> float i1 <+> float i2 <+> float i3 <+> pFill f <+> pColor c
pExp (Triang l1 l2 l3 c)    = 
    text "triang" <+> float l1 <+> float l2 <+> float l3 <+> pColor c
pExp (Polygon l s c) =
    text "poly" <+> int l <+> float s <+> pColor c
pExp (Line i1 i2 i3 c)         = 
    text "line" <+> float i1 <+> float i2 <+> float i3 <+> pColor c
-- Expresiones de Imagenes
pExp (Stack a b) =
    text "stack" <+> pExp a <+> pExp b   
pExp (Offset a b p) =
    text "offset" <+> pExp a <+> pExp b <+> pPoint p   
pExp (Bind a pa b pb) =
    text "bind" <+> pExp a <+> pPoint pa <+> pExp b <+> pPoint pb    
pExp (Rot a b) =
    text "rot" <+> pExp a <+> float b
pExp (RSize a b) =
    text "resize" <+> pExp a <+> float b
pExp (Paint a b) =
    text "paint" <+> pExp a <+> pColor b
pExp (Color r g b) = pColor (Color r g b)
-- Constructores de Acciones
pExp (Move p i) = 
    text "move" <+> pPoint p <+> float i
pExp (Rotate i1 i2)  =
    text "rotate" <+> float i1 <+> float i2
pExp (Scale i1 i2 i3)   =
    text "scale" <+> float i1 <+> float i2 <+> float i3
pExp (Static i)   =
    text "static" <+> float i
pExp (Orbit p a t) =
    text "orbit" <+> pPoint p <+> float a <+> float t
-- Expresiones de Acciones 
pExp (Seq a b)    = pExp a <+> text ";" <+> pExp b
pExp (Par a b)    = pExp a <+> text "||" <+> pExp b
pExp (Loop a i)   = text "loop" <+> pExp a <+> int i
-- Constructores de Animaciones
pExp (Place a p i1 i2)  = 
    text "place" <+> pExp a <+> pPoint p <+> float i1 <+> float i2
-- Variables
pExp (Var n) = pVar n

pDef :: Decl -> Doc
pDef (Decl s t e) = pVar s <+> text ":" <+> pType t <+> text "=" <+> pExp e

pComm :: Comm -> Doc
pComm (Update v a) = pVar v <+> text "<<" <+> pExp a
pComm (Play names) = text "play" <+> text "[" <+> pVars names <+> text "]" 

pTerm :: Terms -> Doc
pTerm (Left c) = pComm c
pTerm (Right d) = pDef d

renderTerm :: Terms -> String
renderTerm = render . pTerm 

renderExp :: Exp -> String
renderExp = render . pExp

renderComm :: Comm -> String
renderComm = render . pComm

renderTy :: Type -> String
renderTy = render . pType

