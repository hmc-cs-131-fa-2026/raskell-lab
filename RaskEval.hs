{-|
Module      : RaskEval
Description : The Raskell evaluator: special forms, function application, and
              primitive operations.

In this lab, the only thing you change in this file is 'primRest' (search for
TODO).
-}
module RaskEval where

import           RaskAST

import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Environments
--------------------------------------------------------------------------------

-- | Look up a name, or explain that it isn't bound.
envLookup :: String -> Env -> Either String Value
envLookup name env =
    case Map.lookup name env of
        Just v  -> Right v
        Nothing -> Left ("unbound identifier: " ++ name)

-- | Bind several names to several values, on top of an existing environment.
-- (New bindings shadow old ones with the same name.)
envExtend :: [String] -> [Value] -> Env -> Env
envExtend names vals env = Map.union (Map.fromList (zip names vals)) env

-- | The environment every program starts in: just the primitives.
initEnv :: Env
initEnv = Map.fromList [ (name, VPrim name f) | (name, f) <- primitives ]

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

-- | Evaluate an expression in an environment, producing either an error
-- message (Left) or a value (Right).
eval :: Env -> SExpr -> Either String Value

-- Numbers and Booleans evaluate to themselves.
eval _   (SNumber n) = Right (VNumber n)
eval _   (SBool b)   = Right (VBool b)

-- A symbol, when evaluated, is a variable: look it up.
eval env (SSymbol name) = envLookup name env

-- () is not a valid expression (quote it to get the empty list).
eval _   (SList []) = Left "empty application: () (did you mean '()?)"

-- Special forms: each has its own evaluation rule.
eval _   (SList (SSymbol "quote"  : parts)) = evalQuote  parts
eval env (SList (SSymbol "lambda" : parts)) = evalLambda env parts

-- Anything else in parentheses is an ordinary application: evaluate the
-- function and ALL the arguments (call by value), then apply.
eval env (SList (fexpr : argexprs)) = do
    f    <- eval env fexpr
    args <- mapM (eval env) argexprs
    apply f args

-- | Apply a function value to already-evaluated arguments.
apply :: Value -> [Value] -> Either String Value
apply (VPrim _ f) args = f args
apply (VClosure closureEnv params body) args
    | length params /= length args =
        Left (arityMessage "procedure" (length params) (length args))
    | otherwise =
        eval (envExtend params args closureEnv) body
apply notAFunction _ =
    Left ("not a procedure: " ++ showValue notAFunction)

--------------------------------------------------------------------------------
-- Special forms
--------------------------------------------------------------------------------

-- | (quote e): return e as data, without evaluating it.
evalQuote :: [SExpr] -> Either String Value
evalQuote [e] = Right (quoteToValue e)
evalQuote es  = Left (arityMessage "quote" 1 (length es))

-- | (lambda (params...) body): make a closure that remembers the CURRENT
-- environment (that's what makes scope lexical).
evalLambda :: Env -> [SExpr] -> Either String Value
evalLambda env [paramList, body] = do
    params <- parameterList paramList
    return (VClosure env params body)
evalLambda _ _ = Left "lambda: expected (lambda (params...) body)"

--------------------------------------------------------------------------------
-- Primitive operations
--
-- Each primitive receives its arguments ALREADY EVALUATED, as a list of values.
--------------------------------------------------------------------------------

-- | Every primitive, by the name a Raskell program uses for it.
primitives :: [(String, [Value] -> Either String Value)]
primitives =
    [ ("cons",  primCons)
    , ("first", primFirst)
    , ("rest",  primRest)
    ]

-- | (cons x xs): a new list whose first element is x and the rest is xs.
primCons :: [Value] -> Either String Value
primCons args = do
    (x, xsv) <- twoArgs "cons" args
    xs <- expectList "cons" xsv
    return (VList (x : xs))

-- | (first xs): the first element of a nonempty list.
primFirst :: [Value] -> Either String Value
primFirst args = do
    xsv <- oneArg "first" args
    xs  <- expectList "first" xsv
    case xs of
        (x : _) -> Right x
        []      -> Left "first: expected a nonempty list, got ()"

-- | (rest xs): everything after the first element of a nonempty list.
--
--   (rest '(a b c))  produces  (b c)
--   (rest '(z))      produces  ()
--   (rest '())       is an error
--   (rest 'a)        is an error
--
-- TODO (Lab 09): replace the line below with a real implementation.
primRest :: [Value] -> Either String Value
primRest _ = Left "rest: not implemented yet"
