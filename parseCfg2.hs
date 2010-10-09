-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (

main
) where
import Text.ParserCombinators.Parsec
import Control.Monad hiding (join)
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language

def = emptyDef{  identStart = letter
               , identLetter = alphaNum <|> char '_'
               }

lexer = T.makeTokenParser def

identifier        = T.identifier    lexer
symbol            = T.symbol        lexer
stringLiteral     = T.stringLiteral lexer
integer           = T.integer       lexer
squares           = T.squares       lexer
braces            = T.braces        lexer
commaSep          = T.commaSep      lexer

file = do
    res <- many (liftM Right (try primary) <|> liftM Left cmdline)
    eof
    return res

primary = do
    spaces
    id <- identifier
    string "="
    lit <- many (noneOf eols)
    optional eol
    return (id,lit)
eols = "\n\r"
eol = oneOf eols
cmdline = do
    spaces
    id <- identifier
    symbol "{"
    options <- many primary
    symbol "}"
    return (id,options)

main = do
    s <- readFile "/home/zsc/tmpd/loongcc_peak_tmp1.cfg"
    putStrLn.show $ parse file "" $ s
    -- "x = \"abc\"  \ny=\"def\""
    --"a = \"th\"\n b = \"  f g \"\n"
