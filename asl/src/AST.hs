module AST where

import Common

data Exp =
      Circle Float Float Fill Exp 
    | Rect Float Float Float Fill Exp
    | Line Float Float Float Exp
    | Triang Float Float Float Exp 
    | Polygon Int Float Exp
    -- Expresiones de Imagenes
    | Stack Exp Exp
    | Offset Exp Exp Point
    | Bind Exp Point Exp Point
    | Rot Exp Float
    | RSize Exp Float
    | Paint Exp Exp
    -- Constructores de Acciones
    | Rotate Float Duration
    | Move Point Duration
    | Scale Float Float Duration
    | Static Duration
    | Orbit Point Float Duration
    -- Expresiones de Acciones
    | Seq Exp Exp
    | Par Exp Exp    
    | Loop Exp Int
    -- Constructores de Animaciones
    | Place Exp Point Float Float
    -- Variables
    | Var Name
    -- Color
    | Color Int Int Int
    deriving (Show, Eq)

data Comm =
    -- Ejecuciones de Acciones
    Update Name Exp
    -- Ejecución de Animaciones
    | Play [Name]

data Decl = Decl Name Type Exp

data Value = I Exp             -- imagen
           | Ac Exp            -- accion
           | An Exp [Exp]      -- animacion
           | C Exp             -- color
           deriving (Show, Eq)


data Scene = Sc Int Int Exp

type Terms = Either Comm Decl

data File = File Scene [Terms] Comm


