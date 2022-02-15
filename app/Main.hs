module Main where

import Data.Map as Map
import Control.Exception (evaluate)
import Debug.Trace

type Context = Map String Value

type TyContext = Map String Type

data Type
  = TInt
  | TBool
  | TArrow {param_type :: Type, body_type :: Type}
  deriving (Show, Eq)

data Expr
  = Var String
  | EInt Int
  | EBool Bool
  | Abs {param :: String, param_type' :: Type, body :: Expr}
  | App {fun :: Expr, arg :: Expr}
  | Prim {op :: Binop, argA :: Expr, argB :: Expr}
  deriving (Show)

data Binop
  = Add
  | Subt
  | Mult
  | Div
  deriving (Show)

data Value
  = VInt Int
  | VBool Bool
  | Closure {context :: Context, param' :: String, body' :: Expr}
  deriving (Show)

data Error
  = ErrorVariableNotFound {variable :: String}
  | ErrorWrongTypeFunc {exprA :: Expr, typeA :: Type, exprB :: Expr, typeB :: Type}
  | ErrorNotLambda {exprA :: Expr, typeA :: Type}
  deriving (Show)

infer :: TyContext -> Expr -> Either Error Type
infer context expr = 
  case expr of
    EInt _ -> Right TInt
    EBool _ -> Right TBool
    Var name -> case Map.lookup name context of
      Just typ -> Right typ
      Nothing -> Left $ ErrorVariableNotFound {variable = name}
    Abs {param = param, param_type' = param_type', body = body} ->
      let context' = Map.insert param param_type' context
       in let body_type = infer context' body
           in case body_type of
                Left err -> Left err
                Right ty -> Right TArrow {param_type = param_type', body_type = ty}
    App {fun = fun, arg = arg} -> do
      fun_type <- infer context fun
      arg_type <- infer context arg
      case fun_type of
        TArrow {param_type = param_type, body_type = body_type} ->
          if param_type == arg_type
            then Right body_type
            else Left $ ErrorWrongTypeFunc {exprA = fun, typeA = fun_type, exprB = arg, typeB = arg_type}
        _ -> Left $ ErrorNotLambda {exprA = fun, typeA = fun_type}
    Prim {op = _, argA = a, argB = b} -> do
      argA_type <- infer context a
      argB_type <- infer context b
      case (argA_type, argB_type) of
        (TInt, TInt) -> Right TInt
        _ -> error "TODO"

interprete :: Context -> Expr -> Maybe Value
interprete context expr = do
  traceM ("context: " ++ show context)
  case expr of
    EInt n -> Just $ VInt n
    EBool b -> Just $ VBool b
    Var name -> Map.lookup name context
    Abs {param = param, param_type' = _, body = body} -> Just $ Closure {context = context, param' = param, body' = body}
    App {fun = fun, arg = arg} ->
      let fun' = interprete context fun
       in let arg' = interprete context arg
           in case fun' of
              Just x -> case x of
                Closure {context = context, param' = param, body' = body} -> case arg' of
                  Just arg -> interprete (Map.insert param arg context) body
                  _ -> Nothing
                VInt v -> Just (VInt v)
                VBool b -> Just (VBool b)
              _ -> Nothing
    Prim {op = op, argA = v, argB = v'} ->
      case (interprete context v, interprete context v') of
        (Just v, Just v') -> Just (interpreteBinop op v v')
        _ -> interprete context v

interpreteBinop :: Binop -> Value -> Value -> Value
interpreteBinop op (VInt v) (VInt v') = case op of
  Add -> VInt $ v + v'
  Subt -> VInt $ v - v'
  Mult -> VInt $ v * v'
  Div -> if v' == 0 then error "Divide by zero" else VInt $ v `div` v'
interpreteBinop op _ _ = error "This operation expects an Int argument"

callTest :: Expr
callTest = 
  App { 
    fun = 
      Abs {
        param = "x", 
        param_type' = TArrow { param_type = TInt, body_type = TInt }, 
        body = Var "x"
      }, 
    arg = 
      Abs {
        param = "y", 
        param_type' = TInt, 
        body = Var "y"
      }
  }

sndTest :: Expr
sndTest =
  Abs
    {
      param = "a",
      param_type' = TInt,
      body =
        Abs { 
          param = "b", 
          param_type' = TArrow { param_type = TInt, body_type = TInt }, 
          body = Var "b" 
        }
    }

sumTest :: Int -> Int -> Expr
sumTest x y =
  App
    { fun =
        App
          { fun =
              Abs
                { param = "x",
                  param_type' = TInt,
                  body =
                    Abs
                      { param = "y",
                        param_type' = TInt,
                        body =
                          Prim
                            { op = Div,
                              argA = Var "x",
                              argB = Var "y"
                            }
                      }
                },
            arg = EInt x
          },
      arg = EInt y
    }

checkType :: Expr -> Either Error Type
checkType e = case infer Map.empty e of
  Left er -> Left er
  Right ty -> Right ty

checkTypeAndInterprete :: Expr -> Either Error (Maybe Value)
checkTypeAndInterprete e = case infer Map.empty e of
  Left er -> Left er
  Right ty -> Right $ interprete Map.empty e

main :: IO()
main = print $ checkType $ sumTest 10 5