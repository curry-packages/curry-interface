------------------------------------------------------------------------------
--- This library defines a parser for Curry interfaces.
------------------------------------------------------------------------------

module CurryInterface.Parser where

import CurryInterface.Types

import Prelude hiding ((*>), (<*), (<*>), (<$>), (<|>), many, empty, some, failure)
import Data.List (init, last)
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
parseModuleIdent = ModuleIdent <$> parseIdentList

parseImportDecls :: Parser [IImportDecl]
parseImportDecls = many parseImportDecl

parseDecls :: Parser [IDecl]
parseDecls = many parseDecl

-- Seems to work
parseImportDecl :: Parser IImportDecl
parseImportDecl = IImportDecl <$> (tokenImport **> parseModuleIdent <** tokenSemicolon)

parseDecl :: Parser IDecl
parseDecl = choice
    [ parseInfixDecl
    , parseHidingDataDecl
    , parseDataDecl
    , parseNewtypeDecl
    , parseTypeDecl
    , parseFunctionDecl
    , parseHidingClassDecl
    , parseClassDecl
    , parseInstanceDecl
    ] <** tokenSemicolon

-- Seems to work
parseInfixDecl :: Parser IDecl
parseInfixDecl = IInfixDecl <$> parseInfix <**> parsePrecedence <**> parseQualIdent

-- Seems to work
parseHidingDataDecl :: Parser IDecl
parseHidingDataDecl = --missing "parseHidingDataDecl"
    HidingDataDecl <$>
        (tokenHiding **> tokenData **> parseQualIdent) <**> 
        yield Nothing <**>
        (map (flip Ident 0) <$> parseTypeVariableList)

parseDataDecl :: Parser IDecl
parseDataDecl =
    convert <$> leftSide <**> (tokenEqual **> rightSide) <**> parsePragma
    where
    convert :: (QualIdent, Maybe KindExpr, [Ident]) -> [ConstrDecl] -> [Ident] -> IDecl
    convert (qi, mk, ids) cs ps = IDataDecl qi mk ids cs ps

    leftSide :: Parser (QualIdent, Maybe KindExpr, [Ident])
    leftSide =
        (,,) <$> (tokenData **> parseQualIdent) <**> parseMaybeKind <**> (map (flip Ident 0) <$> parseTypeVariableList)

    rightSide :: Parser [ConstrDecl]
    rightSide = (:) <$> parseData <**> many (tokenPipe **> parseData)

parseNewtypeDecl :: Parser IDecl
parseNewtypeDecl =
    convert <$> leftSide <**> (tokenEqual **> rightSide) <**> parsePragma
    where
    convert :: (QualIdent, Maybe KindExpr, [Ident]) -> NewConstrDecl -> [Ident] -> IDecl
    convert (qi, mk, ids) nc ps = INewtypeDecl qi mk ids nc ps

    leftSide :: Parser (QualIdent, Maybe KindExpr, [Ident])
    leftSide = (,,) <$> (tokenNewtype **> parseQualIdent) <**> parseMaybeKind <**> (map (flip Ident 0) <$> parseTypeVariableList)

    rightSide :: Parser NewConstrDecl
    rightSide = parseNewtype

parseTypeDecl :: Parser IDecl
parseTypeDecl =
    convert <$> leftSide <**> (tokenEqual **> rightSide)
    where
    convert :: (QualIdent, Maybe KindExpr, [Ident]) -> TypeExpr -> IDecl
    convert (qi, mk, ids) t = ITypeDecl qi mk ids t

    leftSide :: Parser (QualIdent, Maybe KindExpr, [Ident])
    leftSide = (,,) <$> (tokenType **> parseQualIdent) <**> parseMaybeKind <**> (map (flip Ident 0) <$> parseTypeVariableList)

    rightSide :: Parser TypeExpr
    rightSide = parseTypeExpr

parseFunctionDecl :: Parser IDecl
parseFunctionDecl =
    IFunctionDecl <$>
        parseQualIdent <**>
        optional parseMethodPragma <**>
        parseArity <**>
        (tokenTyping **> parseQualTypeExpr)

parseHidingClassDecl :: Parser IDecl
parseHidingClassDecl =
    HidingClassDecl <$>
        (tokenHiding **> tokenClass **> parseContext) <**>
        parseQualIdent <**>
        parseMaybeKind <**>
        (flip Ident 0 <$> parseTypeVariable)

parseClassDecl :: Parser IDecl
parseClassDecl = 
    IClassDecl <$>
        (tokenClass **> parseContext) <**>
        parseQualIdent <**>
        parseMaybeKind <**>
        (flip Ident 0 <$> parseTypeVariable) <**>
        (tokenCurlyBracketL **> parseList tokenSemicolon parseMethodDecl <** tokenCurlyBracketR) <**>
        parsePragma

parseInstanceDecl :: Parser IDecl
parseInstanceDecl =
    IInstanceDecl <$>
        (tokenInstance **> parseContext) <**>
        parseQualIdent <**>
        parseInstanceType <**>
        (tokenCurlyBracketL **> parseList tokenSemicolon parseMethodImpl <** tokenCurlyBracketR) <**>
        optional parseModulePragma

{-
JUST AN OVERVIEW OF THE TYPE
data IDecl
  = IInfixDecl      Infix Precedence QualIdent
  | HidingDataDecl  QualIdent (Maybe KindExpr) [Ident]
  | IDataDecl       QualIdent (Maybe KindExpr) [Ident] [ConstrDecl]  [Ident]
  | INewtypeDecl    QualIdent (Maybe KindExpr) [Ident] NewConstrDecl [Ident]
  | ITypeDecl       QualIdent (Maybe KindExpr) [Ident] TypeExpr
  | IFunctionDecl   QualIdent (Maybe Ident) Arity QualTypeExpr
  | HidingClassDecl Context QualIdent (Maybe KindExpr) Ident
  | IClassDecl      Context QualIdent (Maybe KindExpr) Ident [IMethodDecl] [Ident]
  | IInstanceDecl   Context QualIdent InstanceType [IMethodImpl] (Maybe ModuleIdent)
 deriving (Eq, Read, Show)
-}

parseInfix :: Parser Infix
parseInfix = choice
    [ word "infixl" *> yield InfixL
    , word "infixr" *> yield InfixR
    , word "infix"  *> yield Infix]

parsePrecedence :: Parser Precedence
parsePrecedence = parseInt

parseArity :: Parser Arity
parseArity = parseInt

parseQualIdent :: Parser QualIdent
parseQualIdent = convert <$> parseIdentList
    where
        convert :: [String] -> QualIdent
        convert (ids ++ [id]) =
            let idents = if null ids then Nothing else Just (ModuleIdent ids)
                ident = Ident id 0
            in QualIdent idents ident

parseIdentList :: Parser [String]
parseIdentList =
    toList <$> parenthesize parseOperator <|>
    toList <$> parseOperator <|>
    toList <$> parseIdent <|>
    (++) <$> some (parseIdent <* tokenDot) <*> (toList <$> parseIdent <|> toList <$> parenthesize parseOperator)

parseIdent :: Parser String
parseIdent = (:) <$> check isAlpha anyChar <*> many (check condition anyChar)
    where
    condition c = isDigit c || isAlpha c
{-
parseIdent = some (check condition anyChar)
    where
    condition c = isDigit c || isAlpha c || (c /= '(' && c /= ')' && c /= ';')
-}

parseOperator :: Parser String
parseOperator = check stringCondition (some (check charCondition anyChar))
    where
    charCondition c = elem c allowed
    stringCondition s = not (elem s exceptions)
    allowed = "!#$%&*+./<=>?@\\^|-~:"
    exceptions = ["..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

parseTypeVariable :: Parser String
parseTypeVariable = (:) <$> check isLower anyChar <*> many (check condition anyChar)
    where
    condition c = isAlpha c || isDigit c

parseTypeVariableList :: Parser [String]
parseTypeVariableList = many (skipWhitespace *> parseTypeVariable)

parsePragma :: Parser [Ident]
parsePragma =
    (tokenPragmaL **> tokenPragma **> (map (flip Ident 0) <$> parseIdentList) <** tokenPragmaR) <|> yield []

parseSinglePragma :: Parser () -> Parser Ident
parseSinglePragma token = tokenPragmaL **> token **> (flip Ident 0 <$> parseIdent) <** tokenPragmaR

parseMethodPragma :: Parser Ident
parseMethodPragma = parseSinglePragma tokenPragmaMethod

parseModulePragma :: Parser ModuleIdent
parseModulePragma = tokenPragmaL **> tokenPragmaModule **> parseModuleIdent <** tokenPragmaR

parseData :: Parser ConstrDecl
parseData = choice
    [ parseDataConOp
    , parseDataRecord
    , parseDataConstructor]

parseDataConstructor :: Parser ConstrDecl
parseDataConstructor =
    (uncurry ConstrDecl) <$> parseConstructor

parseDataConOp :: Parser ConstrDecl
parseDataConOp = ConOpDecl <$> parseTypeExpr <**> (flip Ident 0 <$> parseOperator) <**> parseTypeExpr

parseDataRecord :: Parser ConstrDecl
parseDataRecord = 
    RecordDecl <$>
        (flip Ident 0 <$> parseIdent) <**>
        (
            tokenCurlyBracketL **> 
            ((:) <$> parseFieldDecl <**> many (tokenComma **> parseFieldDecl))
            <** tokenCurlyBracketR
        )

parseTypeExpr :: Parser TypeExpr
parseTypeExpr = type0
    where
    -- type0 ::= type1 ['->' type0]
    type0 :: Parser TypeExpr
    type0 = convert <$> type1 <**> (optional (tokenArrow **> type0))
        where
        convert t1 Nothing   = t1
        convert t1 (Just t0) = ArrowType t1 t0
    
    -- type1 ::= [type1] type2
    type1 :: Parser TypeExpr
    type1 = foldl1 ApplyType <$> some (skipWhitespace *> type2)

    -- type2 ::= identType | parenType | bracketType
    type2 :: Parser TypeExpr
    type2 = identType <|> parenType <|> bracketType

    identType :: Parser TypeExpr
    identType = variableType <|> constructorType

    variableType :: Parser TypeExpr
    variableType = VariableType <$> (flip Ident 0 <$> parseTypeVariable)

    constructorType :: Parser TypeExpr
    constructorType = ConstructorType <$> parseQualType

    -- parenType ::= '(' tupleType ')'
    parenType :: Parser TypeExpr
    parenType = tokenParenL **> tupleType <** tokenParenR

    -- tupleType ::= type0
    --            |  type0 ',' type0 { ',' type0 }
    --            |  
    tupleType :: Parser TypeExpr
    tupleType = convert <$> ((:) <$> type0 <**> many (tokenComma **> type0) <|> yield [])
        where
        convert ts = case ts of
            [t] -> t
            _   -> TupleType ts

    -- bracketType ::= '[' type0 ']'
    bracketType :: Parser TypeExpr
    bracketType = ListType <$> (toList <$> (tokenBracketL **> type0 <** tokenBracketR))

-- qualType ::= [context '=>'] type0
parseQualTypeExpr :: Parser QualTypeExpr
parseQualTypeExpr = QualTypeExpr <$> parseContext <**> parseTypeExpr

parseContext :: Parser Context
parseContext = choice [parseNoContext, parseContextList, parseSingleContext]
    where
    parseContextList :: Parser Context
    parseContextList = tokenParenL **> parseList tokenComma parseConstraint <** tokenParenR <** tokenDoubleArrow

    parseSingleContext :: Parser Context
    parseSingleContext = toList <$> parseConstraint <** tokenDoubleArrow

    parseNoContext :: Parser Context
    parseNoContext = yield []


parseConstraint :: Parser Constraint
parseConstraint = 
    Constraint <$> parseQualType <**> parseTypeExpr

parseQualType :: Parser QualIdent
parseQualType = check condition parseQualIdent <|> failure
    where
    condition (QualIdent (Just (ModuleIdent ids)) (Ident id _)) = all (isUpper . head) ids && (isUpper . head) id
    condition (QualIdent Nothing (Ident id _)) = (isUpper . head) id

parseFieldDecl :: Parser FieldDecl
parseFieldDecl =
    FieldDecl <$>
        (map (flip Ident 0) <$> parseIdentList) <**>
        (tokenTyping **> parseTypeExpr)

parseMaybeKind :: Parser (Maybe KindExpr)
parseMaybeKind = yield Nothing

parseNewtype :: Parser NewConstrDecl
parseNewtype = parseNewRecord <|> parseNewConstr

parseNewConstr :: Parser NewConstrDecl
parseNewConstr = 
    parseConstructor *>= convert
    where
    convert :: (Ident, [TypeExpr]) -> Parser NewConstrDecl
    convert (i, ts) = case ts of
        [t] -> yield (NewConstrDecl i t)
        _ -> failure

parseNewRecord :: Parser NewConstrDecl
parseNewRecord =
    NewRecordDecl <$>
        (flip Ident 0 <$> parseIdent) <**>
        (
            tokenCurlyBracketL **>
            parseNewField
            <** tokenCurlyBracketR
        )
    where
    parseNewField :: Parser (Ident, TypeExpr)
    parseNewField = (,) <$> (flip Ident 0 <$> parseIdent) <**> (tokenTyping **> parseTypeExpr)

parseConstructor :: Parser (Ident, [TypeExpr])
parseConstructor = 
    (,) <$>
        (flip Ident 0 <$> parseIdent) <**>
        many (skipWhitespace *> parseTypeExpr)

parseMethodDecl :: Parser IMethodDecl
parseMethodDecl =
    IMethodDecl <$>
        (flip Ident 0 <$> parseIdent) <**>
        optional parseArity <**>
        (tokenTyping **> parseQualTypeExpr)

parseInstanceType :: Parser InstanceType
parseInstanceType = parseTypeExpr

parseMethodImpl :: Parser (Ident, Arity)--IMethodImpl
parseMethodImpl = 
    (,) <$> (flip Ident 0 <$> (parseIdent <|> (tokenParenL **> parseOperator <** tokenParenR))) <**> parseArity

--- Helper Functions
isSpecial :: Char -> Bool
isSpecial c = not (isDigit c) && not (isAlpha c)

toList :: a -> [a]
toList x = [x]

--- Helper parser

parenthesize :: Parser String -> Parser String
parenthesize p = parens <$> (tokenParenL *> p <* tokenParenR)
    where
    parens s = "(" ++ s ++ ")"

parseInt :: Parser Int
parseInt = read <$> some digit 

digit :: Parser Char
digit = check isDigit anyChar

-- |Choose the first succeeding parser from a non-empty list of parsers
choice :: [Parser a] -> Parser a
choice = foldr1 (<|>)

parseList :: Parser () -> Parser a -> Parser [a]
parseList psep pelem = ((:) <$> pelem <**> many (psep **> pelem)) <|> yield []

optional :: Parser a -> Parser (Maybe a)
optional p = Just <$> p <|> yield Nothing

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

tokenPragmaL :: Parser ()
tokenPragmaL = word "{-#"

tokenPragmaR :: Parser ()
tokenPragmaR = word "#-}"

tokenEqual :: Parser ()
tokenEqual = char '='

tokenPipe :: Parser ()
tokenPipe = char '|'

tokenDot :: Parser ()
tokenDot = char '.'

tokenParenL :: Parser ()
tokenParenL = char '('

tokenParenR :: Parser ()
tokenParenR = char ')'

tokenPragma :: Parser ()
tokenPragma = choice
    [ tokenPragmaLanguage
    , tokenPragmaOptions
    , tokenPragmaHiding
    , tokenPragmaMethod
    , tokenPragmaModule]

tokenPragmaLanguage :: Parser ()
tokenPragmaLanguage = word "LANGUAGE"

tokenPragmaOptions :: Parser ()
tokenPragmaOptions = word "OPTIONS"

tokenPragmaHiding :: Parser ()
tokenPragmaHiding = word "HIDING"

tokenPragmaMethod :: Parser ()
tokenPragmaMethod = word "METHOD"

tokenPragmaModule :: Parser ()
tokenPragmaModule = word "MODULE"

tokenBracketL :: Parser ()
tokenBracketL = char '['

tokenBracketR :: Parser ()
tokenBracketR = char ']'

tokenNewtype :: Parser ()
tokenNewtype = word "newtype"

tokenType :: Parser ()
tokenType = word "type"