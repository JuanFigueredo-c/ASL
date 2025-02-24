module Typechecker where

import AST
import Common
import Monads
import Error

expect :: MonadASL m => Type -> Exp -> m()
expect ty e = do t <- getTy e 
                 if t /= ty
                 then failASL $ typeErr e ty t
                 else return () 

getTy :: MonadASL m => Exp -> m Type
getTy (Circle _  _ _ c )  = do expect ColorT c
                               return ImageT 
getTy (Rect _ _ _ _ c )   = do expect ColorT c
                               return ImageT
getTy (Line _ _ _ c )     = do expect ColorT c
                               return ImageT
getTy (Triang _ _ _ c)    = do expect ColorT c
                               return ImageT
getTy (Polygon _ _ c )    = do expect ColorT c
                               return ImageT
-- Expresiones de Imagenes
getTy (Stack i1 i2)                   = do expect ImageT i1
                                           expect ImageT i2
                                           return ImageT
getTy (Offset i1 i2 _)                = do expect ImageT i1
                                           expect ImageT i2
                                           return ImageT
getTy (Bind i1 _ i2 _)                = do expect ImageT i1
                                           expect ImageT i2
                                           return ImageT
getTy (Rot i _)                       = do expect ImageT i
                                           return ImageT
getTy (RSize i _)                     = do expect ImageT i
                                           return ImageT
getTy (Paint i c)                     = do expect ImageT i
                                           expect ColorT c
                                           return ImageT
-- Constructores de Acciones 
getTy (Rotate _ _)                        = return ActionT
getTy (Move _ _ )                         = return ActionT
getTy (Scale _ _ _)                       = return ActionT
getTy (Static  _)                         = return ActionT
getTy (Orbit _ _ _)                       = return ActionT
-- Expresiones de Acciones 
getTy (Seq a1 a2)                         = do expect ActionT a1
                                               expect ActionT a2
                                               return ActionT
getTy (Par a1 a2)                         = do expect ActionT a1
                                               expect ActionT a2   
                                               return ActionT                                      
getTy (Loop a1 _ )                        = do expect ActionT a1
                                               return ActionT
-- Constructores de Animaciones 
getTy (Place i1 _ _ _)                    = do expect ImageT i1
                                               return AnimT
-- Colores
getTy (Color _ _ _)                       = return ColorT
-- Variables
getTy (Var n)  = do val <- lookFor n
                    case val of 
                      Nothing -> failASL $ undefErr n 
                      (Just (ty,_)) -> return ty

tcDecl :: MonadASL m => Decl -> m () 
tcDecl (Decl _ t b) = expect t b 

tcComm :: MonadASL m => Comm -> m ()
tcComm (Update n a) = do val <- lookFor n
                         case val of
                           Nothing -> failASL $ undefErr n
                           (Just (nTy,_)) -> do aTy <- getTy a
                                                if nTy /= AnimT
                                                then failASL $ typeErr (Var n) AnimT nTy
                                                else if aTy /= ActionT
                                                     then failASL $ typeErr a ActionT aTy
                                                     else return ()
tcComm (Play ans) = mapM_ tcAnims ans
    where tcAnims e = do eTy <- getTy (Var e)
                         if eTy /= AnimT
                         then failASL $ typeErr (Var e) AnimT eTy 
                         else return ()
                         
