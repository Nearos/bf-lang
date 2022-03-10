{-# LANGUAGE OverloadedStrings, GADTs #-}
module CodeGen where

import Data.Text (pack, Text)
import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Monad.Loops
import Control.Monad
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import Debug.Trace

import Ast
import Optimise

-- Main function of this file

codeGen :: Program -> Either CompileError Text
codeGen program = case execStateT (genProgram program) (GeneratorState [] [] defaultCompiledFunctions) of
    Left str -> Left $ GenError str
    Right (GeneratorState {context = _, output = _, functions = functions}) -> case M.lookup "main" functions of
        Nothing -> Left $ GenError $ "No main function" ++ show functions
        Just res -> Right $ pack $ optimise $ code res

data CompiledFunction = CompiledFunction {
        args :: [StackEntry],
        code :: String
    }
    deriving Show

defaultCompiledFunctions = M.fromList
                            [("in", CompiledFunction [] ","),
                             ("out", CompiledFunction [StackEntry "a" Byte] "<."),
                             ("inc", CompiledFunction [StackEntry "a" Byte] "<+"),
                             ("dec", CompiledFunction [StackEntry "a" Byte] "<-")]

-- The representation of a type on the stack

class StackEntryDescriptor descriptor where
    goOverDown :: descriptor -> Generator ()
    goOverUp :: descriptor -> Generator ()
    dec :: descriptor -> Generator ()
    inc :: descriptor -> Generator ()
    temp :: descriptor -> Bool
    isDone :: descriptor -> Generator ()


-- Code generation state types

data StackEntry where
    StackEntry :: (Show a, StackEntryDescriptor a) => String -> a -> StackEntry

instance Show StackEntry where
    show (StackEntry name desc) = "|"++ name ++":"++ show desc++"|"

data FrameData = FrameData {
        stackBelow :: [StackEntry],
        stackAbove :: [StackEntry] -- Current memory cell is first in this list
    }
    deriving Show

data GeneratorState = GeneratorState {
        context :: [FrameData],
        output :: String,
        functions :: M.Map String CompiledFunction
    }
    deriving Show

type Generator = StateT GeneratorState (Either String)

-- Stack entry descriptors

data Byte = Byte | Temp
    deriving Show

instance StackEntryDescriptor Byte where
    goOverDown _ = emit "<"
    goOverUp _ = emit ">"
    inc _ = emit "+"
    dec _ = emit "-"
    temp Temp = True
    temp Byte = False
    isDone _ = return ()

data FrameSeparator = FrameSeparator
    deriving Show

instance StackEntryDescriptor FrameSeparator where
    goOverDown _ = return ()
    goOverUp _ = return ()
    inc _ = return ()
    dec _ = return ()
    temp _ = False
    isDone _ = return ()

-- Other functions

isEmpty [] = True
isEmpty _ = False

-- Basic code generation functions
emit :: String -> Generator ()
emit code = modify (\gen -> gen {output = reverse code ++ output gen})

-- Stack Functions

downStack :: Generator ()
downStack = do
    below <- stackBelow . head . context <$> get
    case below of
        [] -> return ()
        x@(StackEntry _ desc):rest -> do
            goOverDown desc
            modify $ moveStackDown rest x
    where
        moveStackDown below moving gen =
            let top:rest= context gen
                top' = FrameData below (moving:stackAbove top)
                context' = top' : rest
            in gen { context = context'}

upStack :: Generator ()
upStack = do
    above <- stackAbove . head . context <$> get
    case above of
        [] -> return ()
        x@(StackEntry _ desc):rest -> do
            goOverUp desc
            modify $ moveStackUp rest x
    where
        moveStackUp above moving gen =
            let top:rest= context gen
                top' = FrameData (moving:stackBelow top) above
                context' = top' : rest
            in gen { context = context'}

pushStack :: StackEntry -> Generator ()
pushStack entry = do
    modify pushStack'
    upStack
    where
        pushStack' gen = gen {
                context = let   top:rest = context gen
                                top' = top {stackAbove = entry:stackAbove top}
                            in top':rest
            }

popStack :: Generator ()
popStack = do
    topStack
    downStack
    modify popStack'
    where
        popStack' gen = gen {
                context = let   top:rest = context gen
                                top' = top {stackAbove = tail $ stackAbove top}
                            in top':rest
            }

removeStack :: Generator () 
removeStack = do
    modify removeStack'
    where
        removeStack' gen = gen {
                context = let   top:rest = context gen
                                top' = top {stackBelow = tail $ stackBelow top}
                            in top':rest
        }

popTemp :: Generator ()
popTemp = do
    topStack
    void $ whileM (pred . stackBelow . head . context <$> get) popStack
    where 
        pred [] = False
        pred (StackEntry _ desc:_) = temp desc 

popUntil :: String -> Generator ()
popUntil sym = do
    topStack
    void $ whileM (pred . stackBelow . head . context <$> get) popStack
    where 
        pred [] = False
        pred (StackEntry name _:_) = name /= sym 

topStack :: Generator ()
topStack = void (whileM (not. isEmpty . stackAbove . head . context <$> get) upStack)

bottomStack :: Generator ()
bottomStack = void $ whileM (not . isEmpty . stackBelow . head . context <$> get) downStack

findOnStack :: String -> Generator (Maybe (Int, StackEntry))
findOnStack symbol = do
    stackFrame <- head . context <$> get
    return $ (\(a, b) -> (a+1, b)) <$> find pred (stackBelow stackFrame) <|> ((\(a, b) -> (-a, b)) <$> find pred (stackAbove stackFrame))
    where
        pred (StackEntry name _) = name == symbol

        find :: (a -> Bool) -> [a] -> Maybe (Int, a)
        find _ [] = Nothing
        find f (x:xs)
            |f x = Just (0, x)
            |otherwise = (\(i, e)-> (i+1, e)) <$> find f xs

focusStack :: String -> Generator Bool
focusStack symbol = do
    index <- findOnStack symbol
    case index of
        Nothing -> return False
        Just (x, _)
            | x > 0 -> True <$ forM [1..x] (const downStack)
            | x < 0 -> True <$ forM [1..(-x)] (const upStack)
            | otherwise -> return True

focusStackOrFail symbol = do
    res <- focusStack symbol
    unless res $ lift $ Left $ "Failed to focus: " ++ symbol ++ " not on stack"

focusStackOrPush symbol = do
    res <- focusStack symbol
    unless res $ do
        topStack
        pushStack $ StackEntry symbol Byte
    downStack

getEntry :: String -> Generator StackEntry
getEntry sym = do
    res <- findOnStack sym
    case res of
        Nothing -> lift $ Left $ "Failed to find symbol " ++ sym ++ " in order to get it's descriptor"
        Just (_, a) -> return a

-- Frame Functions
pushFrame :: Generator ()
pushFrame = modify (\gen -> gen {context = FrameData [] [] : context gen})

popFrame :: Generator ()
popFrame = do
    bottomStack
    modify (\gen -> gen {context = tail $ context gen})

-- Code Gen utils

moveValue :: StackEntryDescriptor desc =>  desc -> Generator a -> Generator b -> Generator ()
moveValue desc goToSource goToDest = do
    goToDest
    emit "[-]"
    goToSource
    isDone desc
    emit "["
    dec desc
    goToDest
    inc desc
    goToSource
    isDone desc
    emit "]"
    topStack

nonlinearMoveToTop :: StackEntryDescriptor desc =>  desc -> Generator a -> Generator ()
nonlinearMoveToTop desc goToSource = do
    topStack
    emit "[-]>[-]<"
    goToSource
    isDone desc
    emit "["
    dec desc
    topStack
    inc desc
    emit ">"
    inc desc
    emit "<"
    goToSource
    isDone desc
    emit "]"
    topStack
    pushStack $ StackEntry "_" Temp
    moveValue desc topStack goToSource
    popStack


-- Code generators 

makeStackArgs = reverse . map (`StackEntry` Byte)

genProgram :: Program -> Generator ()
genProgram [] = return ()

genProgram (FunDef name args body retval:fns) = do
    let stackArgs = makeStackArgs args
    modify (\gen -> gen {context = [FrameData stackArgs []], output = []})
    genFnBody body retval
    modify (\gen -> gen {
        functions = M.insert 
            name 
            (CompiledFunction {
                code = reverse $ output gen,
                CodeGen.args = stackArgs
                }) 
            (functions gen)
            })
    genProgram fns

genFnBody :: [Statement] -> Expression -> Generator ()
genFnBody body retval = do
    mapM_ genStatement body
    genExpression retval
    popTemp
    moveValue Byte topStack bottomStack
    popFrame

genStatement :: Statement -> Generator ()
genStatement (Expr expr) = do
    genExpression expr
    popTemp

genStatement (Assignment name expr) = do
    focusStackOrPush name
    topStack
    genExpression expr
    popTemp
    StackEntry _ d <- getEntry name
    moveValue d topStack (focusStackOrFail name)
    

genStatement (While expr statements) = do
    genExpression expr
    popTemp
    emit "["
    pushStack $ StackEntry ".loop" FrameSeparator

    mapM_ genStatement statements

    popUntil ".loop"
    popStack
    genExpression expr
    popTemp
    emit "]"

-- TODO: IF

genStatement (If expr statements []) = do
    genExpression expr
    popTemp
    emit "["
    pushStack $ StackEntry ".if" FrameSeparator

    mapM_ genStatement statements

    popUntil ".if"
    popStack
    emit "[-]"
    emit "]"

genStatement (If expr trueStatements falseStatements) = do
    pushStack $ StackEntry ".if_temp" Byte
    genExpression expr
    popTemp
    moveValue Byte topStack (focusStackOrFail ".if_temp")
    nonlinearMoveToTop Byte (focusStackOrFail ".if_temp")
    emit "["
    pushStack $ StackEntry ".if" FrameSeparator

    mapM_ genStatement trueStatements

    popUntil ".if"
    popStack
    emit "[-]"
    emit "]"
    nonlinearMoveToTop Byte (focusStackOrFail ".if_temp")
    emit ">[-]<[->+<]+>[-<->[-]]<" -- Should negate it
    emit "["
    pushStack $ StackEntry ".else" FrameSeparator

    mapM_ genStatement falseStatements

    popUntil ".else"
    popStack
    emit "[-]"
    emit "]"
    popUntil ".if_temp"
    popStack


genExpression :: Expression -> Generator ()
genExpression (Variable name) = do
    StackEntry _ d <- getEntry name
    nonlinearMoveToTop d (focusStackOrFail name)
    pushStack $ StackEntry "_" Temp
    downStack

genExpression (IntLit val) = do
    emit "[-]"
    emit $ replicate val '+'
    pushStack $ StackEntry "_" Temp
    downStack

genExpression (CharLit val) = genExpression $ IntLit $ ord val

-- Already generated function
genExpression (Function name subexprs) = do
    function <- M.lookup name . functions <$> get
    CompiledFunction {CodeGen.args = args, code = code}
        <- case function of
            Nothing -> lift $ Left $ "Unbound function " ++ name
            Just res -> return res
    when (length args /= length subexprs) $
        lift $ Left $ "Parity mismatch for function " ++ name
    mapM_ (\a-> genExpression a >> upStack) subexprs
    emit code
    forM_ [1..length args] (const removeStack) 
    pushStack $ StackEntry "_" Temp
    downStack
