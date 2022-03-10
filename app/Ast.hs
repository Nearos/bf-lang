module Ast where

import Text.Parsec

data CompileError = ParseError ParseError | GenError String 
                    deriving Show

type Symbol = String

data Expression     = Variable Symbol
                    | IntLit Int
                    | CharLit Char
                    | Function Symbol [Expression]
                    deriving Show

data Statement  = Assignment Symbol Expression
                | Expr Expression
                | If Expression [Statement] [Statement]
                | While Expression [Statement] 
                deriving Show

data FunDef = FunDef {
        name :: Symbol,
        args :: [Symbol],
        body :: [Statement],
        returnValue :: Expression
    } 
    deriving Show

type Program = [FunDef]