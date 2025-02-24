module Monads where

import Common
import AST
import Error
import Graphics.Gloss hiding (Circle, Line, Scale, Point, Rotate) 
import Control.Monad.State
import Control.Monad.Except

 -- | MONADA PRINCIPAL : MonadASL
data Env = Env
    { vars :: [(Name, (Type, Value))]
    , cantVars :: Int
    , scene :: Scene
    , anims :: [(AnimState, Render)]
}

class (MonadState Env m, MonadError Error m) => MonadASL m where

lookFor :: MonadASL m =>Name -> m (Maybe (Type, Value))
lookFor v = do (Env vs _ _ _ ) <- get 
               return $ lookup v vs

failASL :: MonadASL m => Error -> m a
failASL e = throwError e

modifyASL :: MonadASL m => (Env -> Env) -> m ()
modifyASL = modify 

modifyDeclASL :: MonadASL m => Name -> Type -> Value -> m ()
modifyDeclASL n ty val=  do var <- lookFor n
                            case var of
                                Nothing  -> 
                                    modifyASL(\(Env vs c s a) -> (Env ((n,(ty,val)):vs) (c+1) s a))
                                (Just _) -> failASL $ useErr $ "Variable " ++ (show n) ++ "already defined"


updateAnimationASL :: MonadASL m => Name -> Exp -> m()
updateAnimationASL n action =  modifyASL replace
    where replace :: Env -> Env
          replace (Env vs cv s a) = Env (map update vs) cv s a
          update :: (Name, (Type, Value)) -> (Name, (Type, Value))
          update var@(vname,(vty, vval)) = case vty of
                                            AnimT -> if vname == n
                                                     then let vval' = addAction vval action
                                                          in (vname, (vty, vval'))
                                                     else var
                                            _     -> var
          addAction :: Value -> Exp -> Value
          addAction (An animExp actions) a = 
                (An animExp (actions++[a]))    
          addAction _ _ = undefined -- imposible     

updateAnimsASL :: MonadASL m => [(AnimState,Render)] -> m ()
updateAnimsASL as = modifyASL (\(Env vs cv s _) -> (Env vs cv s as))

addAnimASL :: MonadASL m => (AnimState,Render) -> m ()
addAnimASL a = modifyASL (\(Env vs cv s as) -> (Env vs cv s (a:as)))

type MASL = StateT Env (ExceptT Error IO)

instance MonadASL MASL

staticRender :: Render 
staticRender = (\_ anState -> anState)

initialAnimST :: AnimState
initialAnimST = (AnimState blank 0 0 (Point 0 0) (Point 0 0) 0 0 0 0 0 (Point 0 0))

initialEnv :: Env
initialEnv = Env [] 0 (Sc 0 0 (AST.Color 255 255 255)) [(initialAnimST, staticRender)]

runASL :: MASL a -> IO (Either Error (a, Env))
runASL a = runExceptT (runStateT a initialEnv) 

liftASL :: (MonadASL m, MonadIO m) => MASL a -> m a
liftASL a = do env <- get
               result <- liftIO $ runExceptT (runStateT a env)
               case result of
                  (Left err) -> failASL err
                  (Right (val, _)) -> return val

-- | MONADA ACCIONES : MonadACC

data AccEnv = AccEnv
    { dur :: Duration
    , accs :: [(Duration, Stepper)]
    }   

class (MonadState AccEnv m) => MonadACC m where
    getDur :: m Duration
    getDur = do (AccEnv d _ ) <- get
                return d

    modifyACC :: (AccEnv -> AccEnv) -> m ()
    modifyACC = modify
    
addACC :: MonadACC m => Stepper -> Duration -> m ()
addACC stepper secs = modifyACC add
    where add :: AccEnv -> AccEnv
          add (AccEnv d acs) = (AccEnv (d+secs) (acs++[(secs, stepper)]))

type MACC = StateT AccEnv MASL
instance MonadACC MACC

initialAccEnv :: AccEnv
initialAccEnv = AccEnv 0 []

runACC :: MACC a -> MASL (a, AccEnv)
runACC a = runStateT a initialAccEnv
              