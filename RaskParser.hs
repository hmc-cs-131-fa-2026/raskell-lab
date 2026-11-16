{-|
Module      : RaskParser
Description : Turns Raskell source text into an S-expression ('SExpr').

PROVIDED: you don't need to change this file. It is built with the course's
parser-combinator library ("ParserCombinators"), the same one you used for piq.

The concrete syntax is small:

  * an ATOM is a run of characters with no spaces, parentheses, quotes, or
    semicolons. An atom made only of digits (with an optional leading @-@) is a
    number, @#t@ and @#f@ are Booleans, and anything else is a symbol, so
    @+@, @<=@, @list-library@ and @x2@ are all symbols;
  * a LIST is zero or more expressions inside parentheses;
  * @'e@ is an abbreviation for @(quote e)@;
  * a semicolon starts a comment that runs to the end of the line.
-}
module RaskParser (parseRask, sexpr, stripComments) where

import           ParserCombinators
import           RaskAST

-- | Parse a whole program: exactly one S-expression, with optional whitespace
-- and comments around it. Like every parser built with ParserCombinators, it
-- calls Haskell's @error@ when the input doesn't parse; "Rask" catches that.
parseRask :: String -> SExpr
parseRask source = parse program (stripComments source)

program :: Parser SExpr
program = sexpr <+-> whitespace

-- | One S-expression.
sexpr :: Parser SExpr
sexpr =     quoted
        <|> list
        <|> atom
        <??> "expression expected"

-- | @'e@ means @(quote e)@.
quoted :: Parser SExpr
quoted = sym '\'' <-+> sexpr >>=: \e -> SList [SSymbol "quote", e]

-- | A parenthesized list of expressions.
list :: Parser SExpr
list = openparen <-+> many sexpr <+-> closeparen >>=: SList

-- | A number, a Boolean, or a symbol.
atom :: Parser SExpr
atom = skipws (some (getCharThat atomChar)) >>=: classify
  where
    atomChar c = c `notElem` " \t\r\n()';\""
    classify "#t"                  = SBool True
    classify "#f"                  = SBool False
    classify ('-' : ds@(_:_))
      | all (`elem` ['0'..'9']) ds = SNumber (negate (read ds))
    classify ds@(_:_)
      | all (`elem` ['0'..'9']) ds = SNumber (read ds)
    classify s                     = SSymbol s

-- | Remove comments (from a semicolon to the end of its line).
stripComments :: String -> String
stripComments = unlines . map (takeWhile (/= ';')) . lines
