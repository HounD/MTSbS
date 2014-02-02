
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

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

type Eval alpha = ReaderT Env (ErrorT String (StateT Integer Identity)) alpha

runEval            ::  Env -> Integer -> Eval alpha -> (Either String alpha, Integer)
runEval env st ev  =   runIdentity (runStateT (runErrorT (runReaderT ev env)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)

eval                   ::  Exp -> Eval Value

eval (Lit i)          =   do  tick
                              return $ IntVal i

eval (Var n)          =   do  tick
                              env <- ask
                              case Map.lookup n env of
                                   Nothing -> throwError ("unbound variable: " ++ n)
                                   Just val -> return val                                    

eval (Plus e1 e2)     =   do  tick
                              e1'  <- eval e1
                              e2'  <- eval e2
                              case (e1', e2') of
                                   (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                   _ -> throwError "type error in addition"                                  

eval (Abs n e)        =   do  tick
                              env <- ask
                              return $ FunVal env n e                               

eval (App e1 e2)      =   do  tick
                              val1  <- eval e1
                              val2  <- eval e2
                              case val1 of
                                   FunVal env' n body -> local (const (Map.insert n val2 env')) (eval body)
                                   _ -> throwError "type error in application"                                                                        


exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
--exampleExp = Plus (Lit 1) (Abs "x" (Var "x"))
--exampleExp = Var "x"

main :: IO ()
main = do
  print $ runEval Map.empty (eval exampleExp)
