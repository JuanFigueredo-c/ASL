module Process where 

import Common
import Monads
import AST
import Error

processScene :: MonadASL m => Scene -> m ()
processScene s = modifyASL (\(Env vs c _ a) -> (Env vs c s a))

processDecl :: MonadASL m => Decl -> m ()
processDecl d@(Decl _ _ (Var _)) = processVarDecl d  
processDecl (Decl n ty e) = 
   do let val = case ty of
            ImageT  ->  I e
            ActionT ->  Ac e
            AnimT   ->  An e []
            ColorT  ->  C e
      modifyDeclASL n ty val

processVarDecl :: MonadASL m => Decl -> m ()
processVarDecl (Decl n ty (Var vname)) =  
      do  v <- lookFor vname                           
          case v of
            Nothing          -> failASL $ undefErr n
            (Just (vty,val)) -> do if ty == vty
                                   then modifyDeclASL n ty val
                                   else failASL $ typeErr (Var vname) ty vty
processVarDecl _ = undefined --imposible