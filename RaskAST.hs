{-|
Module      : RaskAST
Description : The two datatypes at the heart of Raskell: syntax (SExpr) and
              runtime values (Value), plus printing and small helpers.

Errors: when a Raskell program is wrong, the interpreter calls Haskell's
@error@ with a message. The REPL catches it and prints @error: ...@.

PROVIDED. You will read this file often, but you shouldn't need to change it.
-}
module RaskAST where

import           Data.Map  (Map)

--------------------------------------------------------------------------------
-- Syntax: what the parser produces
--------------------------------------------------------------------------------

-- | An S-expression: the structure of a program BEFORE evaluation.
--
-- Every Raskell program is one of these. For example, the source text
--
-- > ((lambda (x) x) 'apple)
--
-- parses to
--
-- > SList [ SList [SSymbol "lambda", SList [SSymbol "x"], SSymbol "x"]
-- >       , SList [SSymbol "quote", SSymbol "apple"] ]
--
-- (The parser turns the abbreviation @'e@ into @(quote e)@.)
data SExpr
    = SSymbol String     -- ^ a symbol, such as @x@, @cons@, or @+@
    | SNumber Integer    -- ^ an integer literal, such as @42@ or @-7@
    | SBool   Bool       -- ^ @#t@ or @#f@
    | SList   [SExpr]    -- ^ a parenthesized list, such as @(f a b)@ or @()@
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Values: what evaluation produces
--------------------------------------------------------------------------------

-- | An environment maps names to the values they are bound to.
type Env = Map String Value

-- | A runtime value: the things evaluating a program can produce.
data Value
    = VNumber  Integer                    -- ^ an integer
    | VBool    Bool                       -- ^ a Boolean
    | VSymbol  String                     -- ^ a symbol used as DATA, such as the result of @'apple@
    | VList    [Value]                    -- ^ a proper list of values; @VList []@ is the empty list
    | VClosure Env [String] SExpr         -- ^ a function written in Raskell: its defining
                                          --   environment, its parameter names, and its body
    | VPrim    String ([Value] -> Value)
                                          -- ^ a primitive written in Haskell: its name and
                                          --   what it does to a list of (already evaluated) arguments

-- | Turn a value into the text the REPL prints.
showValue :: Value -> String
showValue (VNumber n)      = show n
showValue (VBool True)     = "#t"
showValue (VBool False)    = "#f"
showValue (VSymbol s)      = s
showValue (VList vs)       = "(" ++ unwords (map showValue vs) ++ ")"
showValue (VClosure _ _ _) = "#<procedure>"
showValue (VPrim name _)   = "#<procedure:" ++ name ++ ">"

-- | Show a value in error messages.
instance Show Value where
    show = showValue

-- | Turn a piece of syntax back into source text (the opposite of parsing).
showSExpr :: SExpr -> String
showSExpr (SSymbol s)    = s
showSExpr (SNumber n)    = show n
showSExpr (SBool True)   = "#t"
showSExpr (SBool False)  = "#f"
showSExpr (SList es)     = "(" ++ unwords (map showSExpr es) ++ ")"

--------------------------------------------------------------------------------
-- Code as data
--------------------------------------------------------------------------------

-- | The value that @(quote e)@ produces: the syntax @e@, viewed as data.
-- Symbols become 'VSymbol's and lists become 'VList's. Nothing is evaluated.
quoteToValue :: SExpr -> Value
quoteToValue (SSymbol s) = VSymbol s
quoteToValue (SNumber n) = VNumber n
quoteToValue (SBool b)   = VBool b
quoteToValue (SList es)  = VList (map quoteToValue es)

--------------------------------------------------------------------------------
-- Helpers for checking values
--
-- Each helper either returns what you asked for, or stops the program with an
-- error message. The String argument is the name of the operation, so the
-- message can say which operation failed.
--------------------------------------------------------------------------------

-- | Check that a value is a number, and extract it.
expectNumber :: String -> Value -> Integer
expectNumber _ (VNumber n) = n
expectNumber op v          = error (op ++ ": expected a number, got " ++ showValue v)

-- | Check that a value is a Boolean, and extract it.
expectBool :: String -> Value -> Bool
expectBool _ (VBool b) = b
expectBool op v        = error (op ++ ": expected a Boolean, got " ++ showValue v)

-- | Check that a value is a list, and extract its elements.
expectList :: String -> Value -> [Value]
expectList _ (VList vs) = vs
expectList op v         = error (op ++ ": expected a list, got " ++ showValue v)

-- | The error message for calling something with the wrong number of arguments.
-- For example, @arityMessage "rest" 1 2@ is @"rest: expected 1 argument, got 2"@.
arityMessage :: String -> Int -> Int -> String
arityMessage op expected actual =
    op ++ ": expected " ++ plural expected "argument" ++ ", got " ++ show actual
  where
    plural 1 word = "1 " ++ word
    plural n word = show n ++ " " ++ word ++ "s"

--------------------------------------------------------------------------------
-- Helpers for checking the SHAPE of special forms
--------------------------------------------------------------------------------

-- | Read the binding list of a @let@ or @letrec@, such as @((x 1) (y (+ 2 3)))@,
-- into pairs of a name and the (unevaluated) expression it is bound to.
bindingList :: String -> SExpr -> [(String, SExpr)]
bindingList form (SList bindings) = forceSpine (map binding bindings)
  where
    binding (SList [SSymbol name, e]) = (name, e)
    binding other = error (form ++ ": bad binding " ++ showSExpr other
                           ++ " (expected (name expression))")
bindingList form other =
    error (form ++ ": expected a list of bindings, got " ++ showSExpr other)

-- | Read the parameter list of a @lambda@, such as @(x y)@, into names.
parameterList :: SExpr -> [String]
parameterList (SList params) = forceSpine (map param params)
  where
    param (SSymbol name) = name
    param other = error ("lambda: parameter is not a symbol: " ++ showSExpr other)
parameterList other =
    error ("lambda: expected a parameter list, got " ++ showSExpr other)

--------------------------------------------------------------------------------
-- Making errors happen on time
--
-- You don't need to read this section. Haskell is lazy: it doesn't compute
-- anything until it's needed. Raskell is NOT lazy. It evaluates every argument
-- before a call, so an error in an argument must stop the program even if the
-- function never uses that argument. These helpers make Haskell finish
-- computing a value right away, so any error inside it happens right away too.
--------------------------------------------------------------------------------

-- | Fully compute a value (but not a closure's environment or body, which may
-- refer to the closure itself).
forceValue :: Value -> Value
forceValue v = check v `seq` v
  where
    check (VNumber n)          = n `seq` ()
    check (VBool b)            = b `seq` ()
    check (VSymbol s)          = length s `seq` ()
    check (VList vs)           = foldr (\x rest -> check x `seq` rest) () vs
    check (VClosure _ ps _)    = foldr (\p rest -> length p `seq` rest) () ps
    check (VPrim name _)       = length name `seq` ()

-- | Fully compute every value in a list, in order.
forceAll :: [Value] -> [Value]
forceAll vs = foldr (\v rest -> forceValue v `seq` rest) () vs `seq` vs

-- | Compute every element of a list, in order (one level deep).
forceSpine :: [a] -> [a]
forceSpine xs = foldr seq () xs `seq` xs
