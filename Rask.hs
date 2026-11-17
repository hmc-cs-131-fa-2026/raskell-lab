{-|
Module      : Main
Description : The Raskell REPL and file runner.

PROVIDED: you don't need to change this file.

Usage (the @rask@ script compiles this file for you when needed):

> ./rask                                run the REPL (:q or :quit to leave)
> ./rask --file prog.rask               evaluate a program and print its value
> ./rask --file prog.rask 6 7           ...if the program's value is a function,
>                                       apply it to each number and print each result
> ./rask --lib lib.rask --file prog.rask
>                                       first load the definitions in lib.rask

A library file contains ONE parenthesized list of definitions:

> ( (name1 expression1)
>   (name2 expression2) ... )

The definitions are evaluated in order, so each one can use the ones before
it. A definition whose expression is a lambda can also call itself, as if it
were bound by letrec. The program then runs in an environment that has all
the library's names in it.
-}
module Main where

import           Control.Exception  (ArithException, ErrorCall (..), Handler (..),
                                     PatternMatchFail, catches, evaluate, try)
import qualified Data.Map           as Map
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure)
import           System.IO

import           RaskAST
import           RaskEval
import           RaskParser

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left usage -> hPutStrLn stderr usage >> exitFailure
        Right opts -> do
            env <- maybe (return initEnv) loadLibrary (optLib opts)
            case optFile opts of
                Nothing   -> repl env
                Just file -> runFile env file (optNumbers opts)

--------------------------------------------------------------------------------
-- Command-line options
--------------------------------------------------------------------------------

data Options = Options
    { optLib     :: Maybe FilePath
    , optFile    :: Maybe FilePath
    , optNumbers :: [Integer]
    }

parseArgs :: [String] -> Either String Options
parseArgs = go (Options Nothing Nothing [])
  where
    go opts []                    = Right opts
    go opts ("--lib"  : f : rest) = go opts { optLib  = Just f } rest
    go opts ("--file" : f : rest) = go opts { optFile = Just f } rest
    go opts (n : rest)
      | Just k <- readInteger n   = go opts { optNumbers = optNumbers opts ++ [k] } rest
    go _ _ = Left "usage: rask [--lib LIBRARY.rask] [--file PROGRAM.rask [NUMBER ...]]"

    readInteger s = case reads s of
                      [(k, "")] -> Just k
                      _         -> Nothing

--------------------------------------------------------------------------------
-- Parsing safely
--------------------------------------------------------------------------------

-- | The parser library reports syntax errors by calling @error@; turn that
-- into an ordinary Left so the REPL keeps running.
safeParse :: String -> IO (Either String SExpr)
safeParse source
  | balance > 0 = return (Left "parse error: missing ) -- the parentheses don't balance")
  | balance < 0 = return (Left "parse error: extra ) -- the parentheses don't balance")
  | otherwise   = do
        result <- try (evaluate (forceSExpr (parseRask source)))
        return $ case result of
            Left (ErrorCall msg) -> Left ("parse error: " ++ msg)
            Right e              -> Right e
  where
    balance      = parenDepth (stripComments source)
    forceSExpr e = length (showSExpr e) `seq` e

-- | How many more ('s than )'s.
parenDepth :: String -> Int
parenDepth s = length (filter (== '(') s) - length (filter (== ')') s)

-- | Print a result the way Raskell shows it.
showResult :: Either String Value -> String
showResult (Left msg) = "error: " ++ msg
showResult (Right v)  = showValue v

--------------------------------------------------------------------------------
-- Evaluating safely
--------------------------------------------------------------------------------

-- | Finish computing a value. If the interpreter stopped with an error
-- (Haskell's @error@, a missing pattern, or division by zero), turn it into an
-- ordinary Left so the REPL keeps running.
safeEval :: Value -> IO (Either String Value)
safeEval v = safely (evaluate (forceValue v))

-- | Run an action, turning an interpreter error into a Left.
safely :: IO a -> IO (Either String a)
safely action =
    (Right <$> action) `catches`
        [ Handler (\(ErrorCall msg) -> return (Left msg))
        , Handler (\e -> return (Left (show (e :: PatternMatchFail))))
        , Handler (\e -> return (Left (show (e :: ArithException))))
        ]

--------------------------------------------------------------------------------
-- Running a file
--------------------------------------------------------------------------------

runFile :: Env -> FilePath -> [Integer] -> IO ()
runFile env file numbers = do
    source <- readFile file
    parsed <- safeParse source
    case parsed of
        Left msg -> putStrLn ("error: " ++ msg)
        Right e  -> do
            result <- safeEval (eval env e)
            case (result, numbers) of
                (_, [])       -> putStrLn (showResult result)
                (Right f, ns) -> mapM_ (\n -> safeEval (apply f [VNumber n]) >>= putStrLn . showResult) ns
                (failed, _)   -> putStrLn (showResult failed)

-- | Load a library file of definitions on top of an environment.
loadLibrary :: FilePath -> IO Env
loadLibrary file = do
    source <- readFile file
    parsed <- safeParse source
    case parsed of
        Left msg -> failWith msg
        Right e  -> do
            pairs <- safely (evaluate (forceSpine (bindingList "library" e)))
            case pairs of
                Left msg -> failWith msg
                Right ps -> defineAll initEnv ps
  where
    failWith msg = hPutStrLn stderr ("error in library " ++ file ++ ": " ++ msg) >> exitFailure

    defineAll env [] = return env
    defineAll env ((name, d) : rest) = do
        result <- safeEval (define env name d)
        case result of
            Left msg -> failWith msg
            Right v  -> defineAll (Map.insert name v env) rest

    -- A lambda can call itself, as if bound by letrec.
    define env name (SList [SSymbol "lambda", paramList, body]) =
        let closure = VClosure (Map.insert name closure env) (parameterList paramList) body
        in  closure
    define env _ e = eval env e

--------------------------------------------------------------------------------
-- The REPL: read, evaluate, print, loop
--------------------------------------------------------------------------------

repl :: Env -> IO ()
repl env = do
    hSetBuffering stdout NoBuffering
    progName <- getProgName
    let reference = progName == "rask-reference"
        prompt    = if reference then "reference> " else "raskell> "
    putStrLn $ if reference
        then "Welcome to the Raskell REFERENCE interpreter. Type :q to quit."
        else "Welcome to Raskell. Type :q to quit."
    loop prompt
  where
    loop prompt = do
        input <- readInput prompt ""
        case input of
            Nothing -> putStrLn ""
            Just line
              | trimmed line `elem` [":q", ":quit"] -> return ()
              | null (trimmed line)                -> loop prompt
              | otherwise -> do
                    parsed <- safeParse line
                    case parsed of
                        Left msg -> putStrLn ("error: " ++ msg)
                        Right e  -> safeEval (eval env e) >>= putStrLn . showResult
                    loop prompt

    -- Keep reading lines until the parentheses balance, so an expression can
    -- span several lines.
    readInput prompt soFar = do
        putStr prompt
        eof <- isEOF
        if eof then return (if null soFar then Nothing else Just soFar) else do
            line <- getLine
            let text = if null soFar then line else soFar ++ "\n" ++ line
            if parenDepth (stripComments text) > 0
                then readInput "   ...> " text
                else return (Just text)

    trimmed = unwords . words
