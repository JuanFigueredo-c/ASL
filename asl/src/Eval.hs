module Eval
 ( evalComm
 , producer
 , Env
 )
where

import AST
import Common
import Monads
import Lib
import Error
import Graphics.Gloss hiding (Circle, Line, Scale, Point, Rotate) 

import Control.Monad.IO.Class



evalComm :: (MonadASL m, MonadIO m) => Comm -> m ()
evalComm (Update animName action) = 
    do acc <- case action of 
                   (Var accName) -> do a <- lookFor accName
                                       case a of
                                            Nothing -> failASL $ undefErr accName -- imposible por typechecker
                                            (Just (_, acc')) -> openExp $ getExpr acc'
                   (acc')        -> openExp acc'
       updateAnimationASL animName acc
evalComm (Play animNames)     = do animations <- mapM evalPlay animNames 
                                   updateAnimsASL animations

evalPlay :: (MonadASL m, MonadIO m) => Name -> m (AnimState, Render)
evalPlay animName   = do res <- lookFor animName 
                         case res of
                              (Nothing) -> failASL $ undefErr animName
                              (Just ( _ , (An animExp actions))) -> do animExp' <- openExp animExp
                                                                       (_ , (AccEnv _ accs')) <- liftASL $ runACC $ runActions actions
                                                                       let renderFunc = createRender accs' 0
                                                                           animState = translateAnim animExp'
                                                                       return (animState, renderFunc)
                              _ -> undefined -- imposible

runActions :: MonadACC m => [Exp] -> m ()
runActions ((action):actions) = do translateAcc action 
                                   runActions actions
runActions []              = return ()

createRender :: [(Duration, Stepper)] -> Duration -> Render
createRender [] _ = staticRender 
createRender ((accDur,stepper):actions) acumAccDur = (\currentSec anState ->
                                            let aState' = stepper anState
                                                (Point vx vy)   = posVel aState'
                                                rotVel'         = rotVel aState'
                                                scVelX'          = scVelX aState'
                                                scVelY'          = scVelY aState'
                                                angVel'         = angVel aState'
                                                pic'            = pic aState'  
                                                (Point cx cy)   = pos aState'
                                                cRot            = rot aState'
                                                cScX            = scX aState'
                                                cScY            = scY aState'
                                                (Point ox oy)   = orbP aState'
                                                cRad            = sqrt ((cx - ox) * (cx - ox)  + (cy - oy) * (cy - oy))   -- calculamos el radio de giro
                                                cAng            = atan2 (cy - oy) (cx - ox)  -- calculamos el angulo de giro actual
                                            in if currentSec <= (acumAccDur + accDur)
                                               then let 
                                                        rot' = cRot + (rotVel' * (currentSec - acumAccDur))
                                                        scX'  = cScX + (scVelX' * (currentSec - acumAccDur))
                                                        scY'  = cScY + (scVelY' * (currentSec - acumAccDur))
                                                        ang' = cAng + (angVel' * (currentSec - acumAccDur)) 
                                                        x' = if angVel' == 0               
                                                             then cx + (vx * (currentSec - acumAccDur))
                                                             else ox + (cRad * cos ang')
                                                        y' = if angVel' == 0
                                                             then cy + (vy * (currentSec - acumAccDur))
                                                             else oy + (cRad * sin ang')
                                                    in  aState' {pic = pic', pos = (Point x' y'), rot = rot', scX = scX', scY = scY'}
                                               else let rot' = cRot + (rotVel' * accDur)
                                                        scX'  = cScX + (scVelX' * accDur)
                                                        scY'  = cScY + (scVelY' * accDur)
                                                        ang' = cAng + (angVel' * accDur)
                                                        x' = if angVel' == 0
                                                             then cx + (vx * accDur)
                                                             else ox + (cRad * cos ang')
                                                        y' = if angVel' == 0
                                                             then cy + (vy * accDur)
                                                             else oy + (cRad * sin ang')
                                                        aState'' = aState' {pic = pic', pos = (Point x' y'), rot = rot', scX = scX', scY = scY' , orbP = (Point 0 0),
                                                                            posVel = (Point 0 0), rotVel = 0, scVelX = 0, scVelY = 0,  angVel = 0}
                                                    in (createRender actions (acumAccDur + accDur)) currentSec aState'')

producer :: AnimState -> Picture
producer anState = let (Point x y) = pos anState
                       rt          = rot anState
                       sx          = scX anState
                       sy          = scY anState
                       p           = pic anState
                  in translate x y $ rotate rt $ scale sx sy p