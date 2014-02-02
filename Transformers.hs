
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader

import Data.Maybe
import qualified Data.Map as Map

type Name   =  String

data Exp    =  Lit Integer
              |  Var Name
              |  Plus Exp Exp
              |  Abs Name Exp
              |  App Exp Exp
              deriving (Show)

data Value  =  IntVal Integer
              |  FunVal Env Name Exp
              deriving (Show)

type Env    =  Map.Map Name Value

type Eval alpha  =   ReaderT Env (ErrorT String Identity) alpha

runEval         :: Env -> Eval alpha -> Either String alpha
runEval env ev  = runIdentity (runErrorT (runReaderT ev env))
 
eval                   ::  Exp -> Eval Value

eval (Lit i)           =   return $ IntVal i

eval (Var n)           =   do  env <- ask
                               case Map.lookup n env of
                                    Nothing -> throwError ("unbound variable: " ++ n)
                                    Just val -> return val

eval (Plus e1 e2)      =   do  e1' <- eval e1
                               e2' <- eval e2
                               case (e1', e2') of
                                    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                    _ -> throwError "type error in addition"

eval (Abs n e)         =   do  env <- ask
                               return $ FunVal env n e

eval  (App e1 e2)      =   do  val1 <- eval e1
                               val2 <- eval e2
                               case val1 of
                                    FunVal env' n body -> local (const (Map.insert n val2 env')) (eval body)
                                    _ -> throwError "type error in application"                                    


--exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
--exampleExp = Plus (Lit 1) (Abs "x" (Var "x"))
exampleExp = Var "x"


main :: IO ()
main = do
  print $ runEval Map.empty (eval exampleExp)
