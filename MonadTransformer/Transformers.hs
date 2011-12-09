module Transformers where

--Examples in "Monad Transformers Step by Step"
--http://www.grabmueller.de/martin/www/pub/Transformers.pdf

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe
import qualified Data.Map as Map

type Name = String      --string

data Exp = Lit Integer  --integer constant
         | Var Name     --variable
         | Plus Exp Exp --addition operation
         | Abs Name Exp --lambda function
         | App Exp Exp  --function application
         deriving (Show)

data Value = IntVal Integer       --integer
           | FunVal Env Name Exp  --higher order function
           deriving (Show)

type Env = Map.Map Name Value  --key-value correspondence

--Straightforward Imprementation
eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) =fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = 
  let IntVal i1 = eval0 env e1
      IntVal i2 = eval0 env e2
  in IntVal (i1+i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = 
  let val1 = eval0 env e1
      val2 = eval0 env e2
  in case val1 of 
    FunVal env' n body -> eval0 (Map.insert n val2 env') body

type Eval1 a = Identity a
runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1 :: Monad m => Env -> Exp -> m Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust (Map.lookup n env)
eval1 env (Plus e1 e2) = do
  IntVal i1 <- eval1 env e1
  IntVal i2 <- eval1 env e2
  return $ IntVal (i1+i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do
  val1 <- eval1 env e1
  val2 <- eval1 env e2
  case val1 of 
    FunVal env' n body -> eval1 (Map.insert n val2 env') body

--2.2 Adding Error Handling
type Eval2 a = ErrorT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runErrorT ev)

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = 
  case Map.lookup n env of
    Nothing -> throwError ("unbound variable: " ++ n)
    Just val -> return val
eval2 env (Plus e1 e2) = do
  do IntVal i1 <- eval2 env e1
     IntVal i2 <- eval2 env e2
     return $ IntVal (i1 + i2)
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do
  FunVal env' n body <- eval2 env e1
  val2 <- eval2 env e2
  eval2 (Map.insert n val2 env') body

--2.3 Hiding the Environment
type Eval3 a = ReaderT Env (ErrorT String Identity) a
runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity (runErrorT (runReaderT ev env))
eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError ("unbound variable: " ++ n)
    Just val -> return val
eval3 (Plus e1 e2) = do
  do e1' <- eval3 e1
     e2' <- eval3 e2
     case (e1', e2') of
       (IntVal i1, IntVal i2) ->
         return $ IntVal (i1 + i2)
       _  -> throwError "type error in addition"
eval3 (Abs n e) = do
  env <- ask
  return $ FunVal env n e
eval3 (App e1 e2) = do
  val1 <- eval3 e1
  val2 <- eval3 e2
  case val1 of
    FunVal env' n body -> 
      local (const (Map.insert n val2 env'))
        (eval3 body)
    _ -> throwError "type error in application"

--2.4 Adding State
type Eval4 a = ReaderT Env (ErrorT String (StateT Integer Identity)) a
runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev = runIdentity (runStateT (runErrorT (runReaderT ev env)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do
   st <- get
   put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do 
  tick 
  return $ IntVal i
eval4 (Var n) = do
  tick
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError ("unbound variable: " ++ n)
    Just val -> return val
eval4 (Plus e1 e2) = do
     tick
     e1' <- eval4 e1
     e2' <- eval4 e2
     case (e1', e2') of
       (IntVal i1, IntVal i2) ->
         return $ IntVal (i1 + i2)
       _  -> throwError "type error in addition"
eval4 (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval4 (App e1 e2) = do
  tick
  val1 <- eval4 e1
  val2 <- eval4 e2
  case val1 of
    FunVal env' n body -> 
      local (const (Map.insert n val2 env'))
        (eval4 body)
    _ -> throwError "type error in application"

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
typeErrorExp = Plus (Lit 1) (Abs "x" (Var "x"))
keyErrorExp = Var "x"

--main = print $ eval0 Map.empty exampleExp
--main = print $ runEval1 (eval1 Map.empty exampleExp)
--main = print $ runEval2 (eval2 Map.empty exampleExp)
--main = print $ runEval3 Map.empty (eval3 exampleExp)
main = print $ runEval4 Map.empty 0 (eval4 exampleExp)
