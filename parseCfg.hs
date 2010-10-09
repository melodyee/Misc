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

file = do
    res <- many (liftM Right (try primary) <|> liftM Left cmdline)
    eof
    return res

skip = skipMany (oneOf " \t")
primary = do
    spaces
    id <- identifier
    skip >> char '=' >> skip
    lit <- many (noneOf eols)
    eol
    return (id,lit)
eols = "\n\r"
eol = oneOf eols
identifier = do
    x <- letter
    xs <- many (alphaNum <|> char '_')
    return (x:xs)
cmdline = do
    spaces
    id <- identifier
    skip >> char '{'
    options <- many primary
    skip >> char '}' >> skip
    eol
    return (id,options)

main = do
    s <- readFile "/home/zsc/tmpd/loongcc_peak_tmp1.cfg"
    putStrLn.show $ parse file "" $ s
    --"a = \"th\"\n b = \"  f g \""
