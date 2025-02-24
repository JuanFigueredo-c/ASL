module Main (main) where

import Lib
import Parse
import Common
import AST
import Eval
import Typechecker
import Process
import Error
import Monads
import Control.Monad.Except
import Graphics.Gloss
import qualified System.Environment as Env


data Options = Options
  { optPrint :: Bool
  , optAST   :: Bool
  , optEval  :: Int
  , optHelp  :: Bool
  }
  deriving Show

main :: IO ()
main = do
  s <- Env.getArgs
  case s of
    [fp] -> do c <- readFile fp
               runPipeline c

    _    -> putStrLn "Uso: Programa <archivo>"

runPipeline :: String -> IO ()
runPipeline input = do case (file_parse input) of 
                        (Ok (File s ts p)) -> do res <- runASL $ processScene s >> runTerms ts >> evalComm p
                                                 case res of
                                                  (Left err) -> printError err
                                                  (Right ( _ , (Env _ _ finalScene animations))) ->
                                                    let (window, background) = translateScene finalScene
                                                    in animate window background $ renderFrame animations
                        (Failed s)       -> do putStrLn s
            where renderFrame :: [(AnimState, Render)] -> Float -> Picture
                  renderFrame animations seconds = pictures $ map (\(anState, renderFunc) -> producer $ renderFunc seconds anState) animations 
                                          

runTerms :: (MonadASL m, MonadIO m) => [Terms] -> m ()
runTerms ts = mapM_ declOrComm ts
    where 
      declOrComm :: (MonadASL m, MonadIO m) => Terms -> m ()
      declOrComm (Left c)  = tcComm c >> evalComm c
      declOrComm (Right d) = tcDecl d >> processDecl d
                              
                                
