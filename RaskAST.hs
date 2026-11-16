{-|
Module      : RaskAST
Description : The two datatypes at the heart of Raskell: syntax (SExpr) and
              runtime values (Value), plus printing and small helpers.

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
    | VPrim    String ([Value] -> Either String Value)
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
-- Helpers for checking arguments
--
-- Each helper either extracts what you asked for (Right) or explains what went
-- wrong (Left). The String argument is the name of the operation, so error
-- messages can say which operation failed.
--------------------------------------------------------------------------------

-- | Check that a value is a number, and extract it.
expectNumber :: String -> Value -> Either String Integer
expectNumber _ (VNumber n) = Right n
expectNumber op v          = Left (op ++ ": expected a number, got " ++ showValue v)

-- | Check that a value is a Boolean, and extract it.
expectBool :: String -> Value -> Either String Bool
expectBool _ (VBool b) = Right b
expectBool op v        = Left (op ++ ": expected a Boolean, got " ++ showValue v)

-- | Check that a value is a list, and extract its elements.
expectList :: String -> Value -> Either String [Value]
expectList _ (VList vs) = Right vs
expectList op v         = Left (op ++ ": expected a list, got " ++ showValue v)

-- | Check that an operation received exactly one argument, and extract it.
oneArg :: String -> [Value] -> Either String Value
oneArg _  [v] = Right v
oneArg op vs  = Left (arityMessage op 1 (length vs))

-- | Check that an operation received exactly two arguments, and extract them.
twoArgs :: String -> [Value] -> Either String (Value, Value)
twoArgs _  [v1, v2] = Right (v1, v2)
twoArgs op vs       = Left (arityMessage op 2 (length vs))

-- | The error message for calling something with the wrong number of arguments.
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
bindingList :: String -> SExpr -> Either String [(String, SExpr)]
bindingList form (SList bindings) = mapM binding bindings
  where
    binding (SList [SSymbol name, e]) = Right (name, e)
    binding other = Left (form ++ ": bad binding " ++ showSExpr other
                          ++ " (expected (name expression))")
bindingList form other =
    Left (form ++ ": expected a list of bindings, got " ++ showSExpr other)

-- | Read the parameter list of a @lambda@, such as @(x y)@, into names.
parameterList :: SExpr -> Either String [String]
parameterList (SList params) = mapM param params
  where
    param (SSymbol name) = Right name
    param other = Left ("lambda: parameter is not a symbol: " ++ showSExpr other)
parameterList other =
    Left ("lambda: expected a parameter list, got " ++ showSExpr other)
