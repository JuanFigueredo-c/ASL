module Error
where

import PPAsl
import Common
import AST

-- Errores
data Error =
      ParseErr
    | TypeErr String
    | UndefErr String
    | UseErr String
    deriving Show

useErr :: String -> Error
useErr s = UseErr s 

undefErr :: Name -> Error 
undefErr n = UndefErr $ ("Variable '" ++ n ++ "' not defined")

typeErr :: Exp -> Type -> Type -> Error
typeErr e expected received = 
    TypeErr $ "\'" ++ (renderExp e) ++ "\'" ++ " Expected to be of type " ++ (renderTy expected)
            ++ " but is of type " ++ (renderTy received)

printError :: Error -> IO ()
printError ParseErr      = putStrLn "Parse error."
printError (TypeErr err) = putStrLn $ "Type Error: " ++ err
printError (UndefErr err) = putStrLn $ "Undef Error: " ++ err
printError (UseErr err) = putStrLn $ "Use Error: " ++ err
