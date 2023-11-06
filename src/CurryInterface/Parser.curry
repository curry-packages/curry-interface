------------------------------------------------------------------------------
--- This library defines a parser for Curry interfaces.
------------------------------------------------------------------------------

module CurryInterface.Parser where

import CurryInterface.Types

import Prelude hiding ((*>), (<*), (<*>), (<$>), many, empty)
import DetParse

--- A parser for the text of a Curry interface.
parseCurryInterface :: String -> Interface
parseCurryInterface txt = case parse parseInterface txt of
    Nothing -> error "Parsing failed"
    Just i -> i
--parseCurryInterface _ = error "parseCurryInterface not yet implemented"

parseInterface :: Parser Interface
parseInterface =
    Interface <$>
        (tokenInterface **> parseModuleIdent) <**>
        (tokenWhere **> tokenCurlyBracketL **> parseImportDecls) <**>
        (parseDecls <** tokenCurlyBracketR)

parseModuleIdent :: Parser ModuleIdent
parseModuleIdent = missing "parseModuleIdent"

parseImportDecls :: Parser [IImportDecl]
parseImportDecls = many parseImportDecl

parseDecls :: Parser [IDecl]
parseDecls = many parseDecl

parseImportDecl :: Parser IImportDecl
parseImportDecl = missing "parseImportDecl"

parseDecl :: Parser IDecl
parseDecl = missing "parseDecl"

--- Helper parser

--- Debug function to fail with an error message of which function is not yet implemented.
missing :: String -> Parser a
missing name = (\_ -> error (name ++ " not yet implemented"))

infixl 4 <**>, <**, **>

(<**>) :: Parser (a -> b) -> Parser a -> Parser b
pa <**> pb = (pa <* skipWhitespace) <*> pb <* skipWhitespace

(**>) :: Parser a -> Parser b -> Parser b
pa **> pb = pa *> skipWhitespace *> pb <* skipWhitespace

(<**) :: Parser a -> Parser b -> Parser a
pa <** pb = pa <* skipWhitespace <* pb <* skipWhitespace

skipWhitespace :: Parser ()
skipWhitespace = many (check isSpace anyChar) *> empty

--- Keyword parser

tokenInterface :: Parser ()
tokenInterface = word "interface"

tokenWhere :: Parser ()
tokenWhere = word "where"

tokenImport :: Parser ()
tokenImport = word "import"

tokenClass :: Parser ()
tokenClass = word "class"

tokenData :: Parser ()
tokenData = word "data"

tokenInstance :: Parser ()
tokenInstance = word "instance"

tokenHiding :: Parser ()
tokenHiding = word "hiding"

tokenCurlyBracketL :: Parser ()
tokenCurlyBracketL = char '{'

tokenCurlyBracketR :: Parser ()
tokenCurlyBracketR = char '}'

tokenSemicolon :: Parser ()
tokenSemicolon = char ';'

tokenComma :: Parser ()
tokenComma = char ','

tokenTyping :: Parser ()
tokenTyping = word "::"

tokenArrow :: Parser ()
tokenArrow = word "->"

tokenDoubleArrow :: Parser ()
tokenDoubleArrow = word "=>"

tokenCommentL :: Parser ()
tokenCommentL = word "{-#"

tokenCommentR :: Parser ()
tokenCommentR = word "#-}"