------------------------------------------------------------------------------
--- This library defines a parser for Curry interfaces.
------------------------------------------------------------------------------

module CurryInterface.Parser where

import CurryInterface.Types

import DetParse

--- A parser for the text of a Curry interface.
parseCurryInterface :: String -> Interface
parseCurryInterface _ = error "parseCurryInterface not yet implemented"


--- Helper parser

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