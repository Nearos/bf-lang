module Main where

import System.Environment
import System.FilePath.Posix(takeBaseName)
import qualified Data.Text as T

import Parser
import CodeGen

removeExtension :: FilePath -> FilePath
removeExtension path = case go path of
        [] -> path --Was no extension
        a -> a
    where
    go path
        | last path == '.' = init path
        | otherwise = go $ init path

main :: IO ()
main = do
    args <- getArgs
    case args of 
        filename:_ -> do
            file <- T.pack <$> readFile filename
            let outFileName = removeExtension filename ++ ".b"
            case parseLang filename file >>= codeGen of
                Left err -> print err
                Right result -> do
                    writeFile outFileName $ T.unpack result
        [] -> do
            putStrLn "bf-lang: fatal error: no input files"
            putStrLn "compilation terminated"