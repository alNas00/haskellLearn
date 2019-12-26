module AST where

import Data.Void

data PTerm = Var String
          | IntConst Integer
          | FloatConst Double
          | BoolConst Bool
          | PyList [PTerm]
          | Neg PTerm
          | Not PTerm 
          | Void
          | BiOp Op PTerm PTerm  
		        deriving (Show)

data Op = And | Or  
          | Less | Greater | Eq | NotEq
          | LessEq | GreaterEq  
          | Add | Sub | Mul | Div
          | Pow | Mod
          | AsAdd | AsSub | AsMul | AsDiv
          | AsPow | AsMod
            deriving (Show)  

data Stmt = Seq [Stmt]
           | Assign String (Either PTerm Stmt)
           | If PTerm Stmt Stmt
           | For PTerm PTerm Stmt
           | While PTerm Stmt
           | Function String [PTerm] Stmt
           | CallFunc String [PTerm]
           | Return PTerm
           | Class String Stmt
           | ListCon [PTerm]
           | LambdaFunc [String] PTerm
             deriving (Show)    
