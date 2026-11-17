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

-- | Look up a name, or stop with an error if it isn't bound.
envLookup :: String -> Env -> Value
envLookup name env =
    case Map.lookup name env of
        Just v  -> v
        Nothing -> error ("unbound identifier: " ++ name)

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

-- | Evaluate an expression in an environment, producing a value. If the
-- program is wrong, evaluation stops with an error message.
eval :: Env -> SExpr -> Value

-- Numbers and Booleans evaluate to themselves.
eval _   (SNumber n) = VNumber n
eval _   (SBool b)   = VBool b

-- A symbol, when evaluated, is a variable: look it up.
eval env (SSymbol name) = envLookup name env

-- () is not a valid expression (quote it to get the empty list).
eval _   (SList []) = error "empty application: () (did you mean '()?)"

-- Special forms: each has its own evaluation rule.
eval _   (SList (SSymbol "quote"  : parts)) = evalQuote  parts
eval env (SList (SSymbol "lambda" : parts)) = evalLambda env parts

-- Anything else in parentheses is an ordinary application: evaluate the
-- function and ALL the arguments (call by value), then apply.
-- (forceValue and forceAll, from RaskAST.hs, make Haskell finish computing
-- them before the call. You don't need to read how they work.)
eval env (SList (fexpr : argexprs)) =
    apply (forceValue (eval env fexpr)) (forceAll (map (eval env) argexprs))

-- | Apply a function value to already-evaluated arguments.
apply :: Value -> [Value] -> Value
apply (VPrim _ f) args = f args
apply (VClosure closureEnv params body) args
    | length params /= length args =
        error (arityMessage "procedure" (length params) (length args))
    | otherwise =
        eval (envExtend params args closureEnv) body
apply notAFunction _ =
    error ("not a procedure: " ++ showValue notAFunction)

--------------------------------------------------------------------------------
-- Special forms
--------------------------------------------------------------------------------

-- | (quote e): return e as data, without evaluating it.
evalQuote :: [SExpr] -> Value
evalQuote [e] = quoteToValue e
evalQuote es  = error (arityMessage "quote" 1 (length es))

-- | (lambda (params...) body): make a closure that remembers the CURRENT
-- environment (that's what makes scope lexical).
evalLambda :: Env -> [SExpr] -> Value
evalLambda env [paramList, body] = VClosure env (parameterList paramList) body
evalLambda _ _ = error "lambda: expected (lambda (params...) body)"

--------------------------------------------------------------------------------
-- Primitive operations
--
-- Each primitive receives its arguments ALREADY EVALUATED, as a list of values.
--------------------------------------------------------------------------------

-- | Every primitive, by the name a Raskell program uses for it.
primitives :: [(String, [Value] -> Value)]
primitives =
    [ ("cons",  primCons)
    , ("first", primFirst)
    , ("rest",  primRest)
    ]

-- | (cons x xs): a new list whose first element is x and the rest is xs.
primCons :: [Value] -> Value
primCons [x, xs] = VList (x : expectList "cons" xs)
primCons args    = error (arityMessage "cons" 2 (length args))

-- | (first xs): the first element of a nonempty list.
primFirst :: [Value] -> Value
primFirst [xs] =
    case expectList "first" xs of
        (x : _) -> x
        []      -> error "first: expected a nonempty list, got ()"
primFirst args = error (arityMessage "first" 1 (length args))

-- | (rest xs): everything after the first element of a nonempty list.
--
--   (rest '(a b c))  produces  (b c)
--   (rest '(z))      produces  ()
--   (rest '())       is an error
--   (rest 'a)        is an error
--
-- TODO (Lab 09): replace the line below with a real implementation.
primRest :: [Value] -> Value
primRest _ = error "rest: not implemented yet"
