module Common
where 

import Graphics.Gloss hiding (Color, Point)

type Name = String

data Point = Point Float Float deriving (Show, Eq)

type Duration = Float

data Fill = Full | Outline deriving (Show, Eq)

data Type = ImageT  
          | ActionT
          | AnimT   
          | ColorT
          deriving (Show, Eq)

data AnimState = AnimState
  { pic :: Picture  -- Imagen 
  , rot :: Float    -- angulo de rotación
  , rotVel :: Float -- velocidad de rotación
  , pos :: Point    -- punto actual
  , posVel :: Point -- velocidades en x e y
  , scX  :: Float    -- factor de escala en X
  , scVelX :: Float  -- velocidad de escala en X
  , scY  :: Float    -- factor de escala en Y
  , scVelY :: Float  -- velocidad de escala en Y
  , angVel :: Float -- velocidad angular
  , orbP :: Point   -- Punto de giro
} deriving (Show, Eq)

type Stepper = AnimState -> AnimState

type Render = Duration -> AnimState -> AnimState


