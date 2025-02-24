module Lib
where

import AST
import Common
import Monads
import Error
import Graphics.Gloss hiding (Circle, Line, Scale, Point, Rotate, Polygon) 
import Data.List (sort)

-- =====================================================
-- || Traduccion de Escena 

translateScene :: Scene -> (Display, Color)
translateScene (Sc width height col) = let window = InWindow "ASL" (width,height) (0,0)
                                           background = translateColor col
                                       in (window, background)

-- =====================================================
-- || Traduccion de Color

translateColor :: Exp -> Color
translateColor (AST.Color r g b) = makeColorI r g b 255
translateColor _                 = undefined -- imposible

-- =====================================================
-- ||  Traductores de Imagenes

translateImg :: Exp -> Picture
-- Constructores de imagenes
translateImg (Circle rad thick fill col) =
    let col' = translateColor col
    in  case fill of
            Full    -> color col' $ circleSolid rad
            Outline -> color col' $ thickCircle rad thick
translateImg (Rect width height thick fill col) =
    let col' = translateColor col
    in case fill of
            Full    -> color col' $ rectangleSolid width height
            Outline -> color col' $ thickRectangle width height thick
translateImg (Triang a b c col) =
    let col' = translateColor col
    in color col' $ triangleSolid a b c
translateImg (Polygon sides len col) =
    let col' = translateColor col
    in color col' $ polygonSolid sides len
translateImg (Line len rotation thick col) =
    let col' = translateColor col
    in rotate rotation $ color col' $ rectangleSolid len thick
-- Operadores de imagenes
translateImg (Stack pic1 pic2) =
    let pic1' = translateImg pic1
        pic2' = translateImg pic2
    in pictures [ pic2', pic1'] 
translateImg (Offset pic1 pic2 (Point offx offy)) =
    let pic1' = translateImg pic1
        pic2' = translateImg pic2
    in pictures ([pic1'] ++ [(translate offx offy pic2')]) 
translateImg (Bind pic1 (Point offx1 offy1) pic2 (Point offx2 offy2)) =
    let pic1' = translateImg pic1
        pic2' = translateImg pic2
    in pictures ([translate offx1 offy1 pic1'] ++ [translate offx2 offy2 pic2']) 
translateImg (Rot p r) =
    let pic' = translateImg p
    in rotate r $ pic'
translateImg (RSize p f) =
    let pic' = translateImg p
    in scale f f $ pic'
translateImg (Paint p col) =
    translateImg $ recolor col p

translateImg _ = undefined -- imposible

recolor :: Exp -> Exp -> Exp
recolor col (Stack p1 p2) = let p1' = recolor col p1
                                p2' = recolor col p2
                            in (Stack p1' p2')
recolor col (Offset p1 p2 o) = let p1' = recolor col p1
                                   p2' = recolor col p2
                               in (Offset p1' p2' o)
recolor col (Bind p1 o1 p2 o2) = let p1' = recolor col p1
                                     p2' = recolor col p2
                                 in (Bind p1' o1 p2' o2)
recolor col (Rot p r) = (Rot (recolor col p ) r)
recolor col (RSize p f) = (RSize (recolor col p) f) 
recolor col (Paint p _) = recolor col p
recolor col (Circle a b c _ ) = (Circle a b c col)
recolor col (Rect a b c d _ ) = (Rect a b c d col)
recolor col (Line a b c _ )   = (Line a b c col)
recolor col (Triang a b c _ ) = (Triang a b c col)
recolor col (Polygon a b _ )  = (Polygon a b col)
recolor _ _  = undefined -- imposible


thickRectangle :: Float -> Float -> Float -> Picture
thickRectangle width height thick = 
    pictures $ ([ translate x y $ rectangleSolid thick height
             | x <- [(-width/2) + thick/2, (width/2) - thick/2]
             , y <- [0.0, 0.0]
             ]) ++
            [ translate x y $ rectangleSolid width thick
            | x <- [0.0, 0.0]
            , y <- [(-height/2) + thick/2, (height/2) - thick/2]
            ]

polygonSolid :: Int -> Float -> Picture
polygonSolid sides len
  | sides < 3 = blank
  | otherwise = polygon vertices
  where
    angle = 2 * pi / fromIntegral sides
    radius = len / (2 * sin (pi / fromIntegral sides))
    vertices = [(radius * cos (angle * fromIntegral i), radius * sin (angle * fromIntegral i)) | i <- [0..(sides - 1)]]

isTriangle :: Float -> Float -> Float -> Bool
isTriangle a b c =
  a + b > c && a + c > b && b + c > a

triangleSolid :: Float -> Float -> Float -> Picture
triangleSolid a b c = 
    if isTriangle a b c 
    then translate (-centroideX) (-centroideY) $ polygon vertices
    else blank
  where
    [x, y, z] = sort [a, b, c] -- z es el lado más largo
    vertices = [(0, 0), (x, 0), (cx, cy)]
    cosTheta = (x*x + z*z - y*y) / (2 * x * z) -- Coseno del ángulo opuesto a y
    sinTheta = sqrt (1 - (cosTheta * cosTheta))          -- Seno del mismo ángulo
    cx = z * cosTheta                         -- Coordenada x del tercer vértice
    cy = z * sinTheta                         -- Coordenada y del tercer vértice
    centroideX = (0 + x + cx) / 3
    centroideY = (0 + 0 + cy) / 3

-- =====================================================
-- | Traductores de acciones

translateAcc :: MonadACC m => Exp -> m ()
-- Constructores de acciones
translateAcc e@(Rotate _ secs) = 
    do stepperRot <- getStepper e  
       addACC stepperRot secs
translateAcc e@(Move _ secs) = 
    do stepperMov <- getStepper e  
       addACC stepperMov secs 
translateAcc e@(Scale _ _ secs) = 
    do stepperSc <- getStepper e
       addACC stepperSc secs
translateAcc e@(Static secs) = 
    do stepperSt <- getStepper e
       addACC stepperSt secs
translateAcc e@(Orbit _ _ secs) =
    do stepperOrb <- getStepper e
       addACC stepperOrb secs
-- Operadores de acciones
translateAcc (Seq acc1 acc2) =
    do translateAcc acc1
       translateAcc acc2
translateAcc (Par acc1 acc2) =
    do stepAcc2 <- getStepper acc2
       propagateStepper acc1 stepAcc2            
translateAcc (Loop acc 1)     = translateAcc acc
translateAcc (Loop acc iters) = do translateAcc acc
                                   translateAcc (Loop acc (iters-1))
translateAcc _ = undefined -- imposible


lookForTranslation :: Exp -> Bool
lookForTranslation (Par a b) =  let a' = lookForTranslation a
                                    b' = lookForTranslation b
                                in a' || b'
lookForTranslation (Seq a b) =  let a' = lookForTranslation a
                                    b' = lookForTranslation b
                                in a' || b'
lookForTranslation (Move _ _) = True
lookForTranslation (Orbit _ _ _) = True
lookForTranslation _ = False

lookForRotate :: Exp -> Bool
lookForRotate (Par a b) =  let a' = lookForRotate a
                               b' = lookForRotate b
                           in a' || b'
lookForRotate (Seq a b) =  let a' = lookForRotate a
                               b' = lookForRotate b
                           in a' || b'
lookForRotate (Rotate _ _) = True
lookForRotate _ = False

lookForScale :: Exp -> Bool
lookForScale (Par a b) =  let a' = lookForScale a
                              b' = lookForScale b
                          in a' || b'
lookForScale (Seq a b) =  let a' = lookForScale a
                              b' = lookForScale b
                          in a' || b'
lookForScale (Scale _ _ _) = True
lookForScale _ = False

propagateStepper :: MonadACC m => Exp -> Stepper -> m ()
propagateStepper (Seq a b) stepper = do propagateStepper a stepper
                                        propagateStepper b stepper
propagateStepper (Par a b) stepper = do stepperB <- getStepper b
                                        propagateStepper a (stepperB . stepper)
propagateStepper (Loop a iters) stepper = do let a' = loop2Seq a iters
                                             propagateStepper a' stepper
propagateStepper a stepper = do stepperA <- getStepper a
                                let stepper' = stepperA . stepper
                                    durA = getDuration a
                                addACC stepper' durA




getStepper :: MonadACC m => Exp -> m Stepper 
getStepper (Rotate r secs) = let rotVel' = r/secs
                             in return (\animState -> animState {rotVel = rotVel'}) -- ^ rota rot/secs grados cada segundo
getStepper (Move (Point x y) secs) = return (\animState ->  let (Point cx cy) = pos animState
                                                                vx' = (x - cx) / secs
                                                                vy' = (y - cy) / secs
                                                            in animState {posVel = (Point vx' vy')}) 
getStepper (Scale factorX factorY secs) = return (\animState -> let scX' = scX animState
                                                                    scY' = scY animState
                                                                    scVelX' = (factorX-scX')/secs
                                                                    scVelY' = (factorY-scY')/secs
                                                                in animState {scVelX = scVelX', scVelY = scVelY'})
getStepper (Static _ ) =  return (\animState -> animState)
getStepper (Orbit p deg secs) = let angVel' = (deg * (pi / 180) / secs) 
                                          in  return (\animState -> animState {angVel = angVel', orbP = p})
getStepper (Par ac1 ac2) = do stepper1 <- getStepper ac1
                              stepper2 <- getStepper ac2
                              return (stepper2.stepper1)

getStepper (Seq ac1 ac2) = do stepper1 <- getStepper ac1
                              stepper2 <- getStepper ac2
                              return (stepper2.stepper1)

getStepper (Loop ac 1)     = getStepper ac
getStepper (Loop ac iters) = do stepper <- getStepper ac
                                stepper' <- getStepper (Loop ac (iters-1))
                                return (stepper'.stepper)
getStepper _ = undefined --imposible


-- =====================================================
-- || Traduccion de Animaciones

translateAnim :: Exp -> AnimState
translateAnim (Place image ps rt factor) = 
        let p = translateImg image
        in initialAnimST { pic = p, rot = rt, pos = ps, scX = factor, scY = factor}

translateAnim _ = undefined --imposible

-- Funcionalidades auxiliares

loop2Seq :: Exp -> Int -> Exp
loop2Seq acc 1 = acc
loop2Seq acc iters = let acc' = loop2Seq acc (iters - 1)
                     in Seq acc acc'

getDuration :: Exp -> Duration
getDuration (Rotate _ d)    = d
getDuration (Move _ d)      = d
getDuration (Scale _ _ d)   = d
getDuration (Static d)      = d
getDuration (Orbit _ _ d)   = d
getDuration (Seq ac1 ac2) = let ac1' = getDuration ac1
                                ac2' = getDuration ac2
                            in ac1' + ac2'
getDuration (Par ac1 ac2) = let ac1' = getDuration ac1
                                ac2' = getDuration ac2
                            in max ac1' ac2'
getDuration (Loop ac iters) = let ac' = getDuration ac
                              in (fromIntegral iters) * ac'
getDuration _ = undefined -- imposible

setDuration :: Duration -> Exp -> Exp
setDuration i (Rotate rotation _ )  = (Rotate rotation i)
setDuration i (Move point _ )  = (Move point i)
setDuration i (Scale factorX factorY _) = (Scale factorX factorY i)
setDuration i (Static _)       = (Static i)
setDuration i (Orbit p a _)    = (Orbit p a i)
setDuration i (Seq ac1 ac2)    = let ac1' = setDuration i ac1 
                                     ac2' = setDuration i ac2
                                 in (Seq ac1' ac2')
setDuration i (Par ac1 ac2)    = let ac1' = setDuration i ac1 
                                     ac2' = setDuration i ac2
                                 in (Par ac1' ac2')
setDuration i (Loop ac iters)  = let acTime = i / (fromIntegral iters)
                                     ac' = setDuration acTime ac
                                 in (Loop ac' iters)
setDuration _ _ = undefined  -- imposible

getExpr :: Value -> Exp
getExpr (I e)    = e
getExpr (Ac e)   = e
getExpr (An e _) = e
getExpr (C e) = e

openExp :: MonadASL m => Exp -> m Exp
openExp (Var name) = do res <- lookFor name
                        case res of
                             Nothing -> failASL $ undefErr name -- imposible por typechecker
                             (Just (_, val)) -> openExp $ getExpr val
openExp (Circle a b c col) = do col' <- openExp col
                                return (Circle a b c col')
openExp (Rect a b c d col) = do col' <- openExp col
                                return (Rect a b c d col')
openExp (Triang a b c col) = do col' <- openExp col
                                return (Triang a b c col')
openExp (Polygon a b col) =  do col' <- openExp col
                                return (Polygon a b col')
openExp (Line a b c col)   = do col' <- openExp col
                                return (Line a b c col')
openExp (Stack pic1 pic2)  = do pic1' <- openExp pic1
                                pic2' <- openExp pic2
                                return (Stack pic1' pic2')
openExp (Offset pic1 pic2 p)  = do pic1' <- openExp pic1
                                   pic2' <- openExp pic2
                                   return (Offset pic1' pic2' p)
openExp (Bind pic1 p1 pic2 p2)  = do pic1' <- openExp pic1
                                     pic2' <- openExp pic2
                                     return (Bind pic1' p1 pic2' p2)
openExp (Rot pic1 r)          =  do pic1' <- openExp pic1 
                                    return (Rot pic1' r)
openExp (RSize pic1 r)        =  do pic1' <- openExp pic1 
                                    return (RSize pic1' r)
openExp (Paint pic1 c)        =  do pic1' <- openExp pic1
                                    col' <- openExp c
                                    return (Paint pic1' col')
openExp (Rotate a b)       = return (Rotate a b)
openExp (Move a b)         = return (Move a b)
openExp (Scale a b c)      = return (Scale a b c)
openExp (Static a)         = return (Static a)
openExp (Orbit a b c)      = return (Orbit a b c)
openExp (Seq acc1 acc2)    = do acc1' <- openExp acc1
                                acc2' <- openExp acc2
                                return (Seq acc1' acc2')
openExp (Par acc1 acc2)    = do acc1' <- openExp acc1
                                acc2' <- openExp acc2
                                validatePar acc1' acc2'
                                return (Par acc1' acc2')
openExp (Loop acc a)       = do acc' <- openExp acc
                                return (Loop acc' a)
openExp (Place img a b c)  = do img' <- openExp img
                                return (Place img' a b c)
openExp (AST.Color a b c)  = return (AST.Color a b c)                            

validatePar :: MonadASL m => Exp -> Exp -> m()
validatePar _ (Seq _ _ ) = failASL $ useErr "Second argument of '||' cannot be compound."
validatePar _ (Par _ _ ) = failASL $ useErr "Second argument of '||' cannot be compound."
validatePar acc1 (Move _ _) = if lookForTranslation acc1
                              then failASL $ useErr "Cannot parallelize two translation actions."
                              else return ()
validatePar acc1 (Orbit _ _ _) = if lookForTranslation acc1
                                 then failASL $ useErr "Cannot parallelize two translation actions."
                                 else return ()
validatePar acc1 (Rotate _ _ ) = if lookForRotate acc1
                                 then failASL $ useErr "Cannot parallelize two rotation actions."
                                 else return ()
validatePar acc1 (Scale _ _ _ ) = if lookForScale acc1
                                  then failASL $ useErr "Cannot parallelize two scale actions."
                                  else return ()
validatePar _  _  = return ()