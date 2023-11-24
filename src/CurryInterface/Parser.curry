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
parseCurryInterface txt = case parse interface txt of
    Nothing -> error "Parsing failed"
    Just i -> i
--parseCurryInterface _ = error "parseCurryInterface not yet implemented"

--- A parser for a Curry interface.
interface :: Parser Interface
interface =
    Interface <$>
        (tokenInterface **> moduleIdent) <**>
        (tokenWhere **> tokenCurlyBracketL **> importDecls) <**>
        (decls <** tokenCurlyBracketR)

--- A parser for a Module Identifier.
moduleIdent :: Parser ModuleIdent
moduleIdent = ModuleIdent <$> identList

--- A parser for a list of Import Declarations.
importDecls :: Parser [IImportDecl]
importDecls = many importDecl

--- A parser for a list of Declarations.
decls :: Parser [IDecl]
decls = many decl

--- A parser for an Import Declaration.
importDecl :: Parser IImportDecl
importDecl = IImportDecl <$> (tokenImport **> moduleIdent <** tokenSemicolon)

--- A parser for a Declaration.
decl :: Parser IDecl
decl = choice
    [ hidingDecl
    , infixDecl
    , dataDecl
    , newtypeDecl
    , typeDecl
    , classDecl
    , instanceDecl
    , functionDecl
    ] <** tokenSemicolon

--- A parser for an Infix Declaration | Infix Arity Op
infixDecl :: Parser IDecl
infixDecl = IInfixDecl <$> iInfix <**> precedence <**> qualIdent

hidingDecl :: Parser IDecl
hidingDecl = tokenHiding **> (hidingDataDecl <|> hidingClassDecl)

--- A parser for a Hiding Data Declaration | hiding data QualIdent [KindExpr] TypeVariableList
hidingDataDecl :: Parser IDecl
hidingDataDecl =
    HidingDataDecl <$>
        (tokenData **> qualIdent) <**> 
        yield Nothing <**>
        (map (flip Ident 0) <$> typeVariableList)

--- A parser for a Hiding Class Declaration | hiding class [Context =>] QualIdent [KindExpr] TypeVariable
hidingClassDecl :: Parser IDecl
hidingClassDecl =
    HidingClassDecl <$>
        (tokenClass **> context) <**>
        qualIdent <**>
        optional kind <**>
        (flip Ident 0 <$> typeVariable)

--- A parser for a Data Declaration | data QualIdent [KindExpr] TypeVariableList = ConstrDeclList
dataDecl :: Parser IDecl
dataDecl =
    convert <$> leftSide <**> (tokenEqual **> rightSide) <**> pragma
    where
    convert :: (QualIdent, Maybe KindExpr, [Ident]) -> [ConstrDecl] -> [Ident] -> IDecl
    convert (qi, mk, ids) cs ps = IDataDecl qi mk ids cs ps

    leftSide :: Parser (QualIdent, Maybe KindExpr, [Ident])
    leftSide =
        (,,) <$> (tokenData **> qualIdent) <**> optional kind <**> (map (flip Ident 0) <$> typeVariableList)

    rightSide :: Parser [ConstrDecl]
    rightSide = (:) <$> constrDecl <**> many (tokenPipe **> constrDecl)

--- A parser for a Newtype Declaration | newtype QualIdent [KindExpr] TypeVariableList = Newtype
newtypeDecl :: Parser IDecl
newtypeDecl =
    convert <$> leftSide <**> (tokenEqual **> rightSide) <**> pragma
    where
    convert :: (QualIdent, Maybe KindExpr, [Ident]) -> NewConstrDecl -> [Ident] -> IDecl
    convert (qi, mk, ids) nc ps = INewtypeDecl qi mk ids nc ps

    leftSide :: Parser (QualIdent, Maybe KindExpr, [Ident])
    leftSide = (,,) <$> (tokenNewtype **> qualIdent) <**> optional kind <**> (map (flip Ident 0) <$> typeVariableList)

    rightSide :: Parser NewConstrDecl
    rightSide = iNewtype

--- A parser for a Type Declaration | type QualIdent [KindExpr] TypeVariableList = TypeExpr
typeDecl :: Parser IDecl
typeDecl =
    convert <$> leftSide <**> (tokenEqual **> rightSide)
    where
    convert :: (QualIdent, Maybe KindExpr, [Ident]) -> TypeExpr -> IDecl
    convert (qi, mk, ids) t = ITypeDecl qi mk ids t

    leftSide :: Parser (QualIdent, Maybe KindExpr, [Ident])
    leftSide = (,,) <$> (tokenType **> qualIdent) <**> optional kind <**> (map (flip Ident 0) <$> typeVariableList)

    rightSide :: Parser TypeExpr
    rightSide = typeExpr

--- A parser for a Function Declaration | QualIdent [MethodPragma] Arity :: QualTypeExpr
functionDecl :: Parser IDecl
functionDecl =
    IFunctionDecl <$>
        qualIdent <**>
        optional methodPragma <**>
        arity <**>
        (tokenTyping **> qualTypeExpr)

--- A parser for a Class Declaration | class [Context =>] QualIdent [KindExpr] TypeVariable \{ MethodList \} [Pragma]
classDecl :: Parser IDecl
classDecl = 
    IClassDecl <$>
        (tokenClass **> context) <**>
        qualIdent <**>
        optional kind <**>
        (flip Ident 0 <$> typeVariable) <**>
        (tokenCurlyBracketL **> parseList tokenSemicolon methodDecl <** tokenCurlyBracketR) <**>
        pragma

--- A parser for an Instance Declaration | instance [Context =>] QualIdent InstanceType \{ MethodImplList \} [ModulePragma]
instanceDecl :: Parser IDecl
instanceDecl =
    IInstanceDecl <$>
        (tokenInstance **> context) <**>
        qualIdent <**>
        instanceType <**>
        (tokenCurlyBracketL **> parseList tokenSemicolon methodImpl <** tokenCurlyBracketR) <**>
        optional modulePragma

--- A parser for an Infix expression | {infixl | infixr | infix}
iInfix :: Parser Infix
iInfix = word "infix" *> choice [char 'l' *> yield InfixL, char 'r' *> yield InfixR, yield Infix]

--- A parser for a Precedence
precedence :: Parser Precedence
precedence = parseInt

--- A parser for an Arity
arity :: Parser Arity
arity = parseInt

--- A parser for a Qualified Identifier | [IdentList .] {Ident | Operator | \( Operator \)}
qualIdent :: Parser QualIdent
qualIdent = QualIdent <$> (optional (moduleIdent <* tokenDot)) <*> (flip Ident 0 <$> finalIdent)
    where
    finalIdent :: Parser String
    finalIdent = (tokenParenL *> operator <* tokenParenR <|> operator <|> ident)

--- A parser for a List of Identifiers | Ident [. IdentList]
identList :: Parser [String]
identList = parseList tokenDot ident

--- A parser for an Identifier (not operator)
ident :: Parser String
ident = (:) <$> check isAlpha anyChar <*> many (check condition anyChar)
    where
    condition c = isDigit c || isAlpha c

--- A parser for an Operator
operator :: Parser String
operator = check stringCondition (some (check charCondition anyChar))
    where
    charCondition c = elem c allowed
    stringCondition s = not (elem s exceptions)
    allowed = "!#$%&*+./<=>?@\\^|-~:"
    exceptions = ["..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

--- A parser for a Type Variable
typeVariable :: Parser String
typeVariable = (:) <$> check isLower anyChar <*> many (check condition anyChar)
    where
    condition c = isAlpha c || isDigit c

--- A parser for a Type Variable List | TypeVariable [TypeVariableList]
typeVariableList :: Parser [String]
typeVariableList = many (skipWhitespace *> typeVariable)

--- A parser for a Pragma | ADD SYNTAX DESCRIPTION
pragma :: Parser [Ident]
pragma =
    (tokenPragmaL **> tokenPragma **> (map (flip Ident 0) <$> identList) <** tokenPragmaR) <|> yield []

--- A parser for a Method Pragma | ADD SYNTAX DESCRIPTION
methodPragma :: Parser Ident
methodPragma = parseSinglePragma tokenPragmaMethod

--- A parser for a Module Pragma | ADD SYNTAX DESCRIPTION
modulePragma :: Parser ModuleIdent
modulePragma = tokenPragmaL **> tokenPragmaModule **> moduleIdent <** tokenPragmaR

--- A parser for a Constructor Declaration
constrDecl :: Parser ConstrDecl
constrDecl = choice
    [ constrDeclOp
    , constrDeclRecord
    , constrDeclSimple]

--- A parser for a simple Constructor Declaration | Ident TypeExprList
constrDeclSimple :: Parser ConstrDecl
constrDeclSimple =
    (uncurry ConstrDecl) <$> parseConstructor

--- A parser for an Operator Constructor Declaration | TypeExpr Op TypeExpr
constrDeclOp :: Parser ConstrDecl
constrDeclOp = ConOpDecl <$> typeExpr <**> (flip Ident 0 <$> operator) <**> typeExpr

--- A parser for a Record Constructor Declaration | Ident \{ FieldDeclList \}
constrDeclRecord :: Parser ConstrDecl
constrDeclRecord = 
    RecordDecl <$>
        (flip Ident 0 <$> ident) <**>
        (
            tokenCurlyBracketL **> 
            ((:) <$> fieldDecl <**> many (tokenComma **> fieldDecl))
            <** tokenCurlyBracketR
        )

--- A parser for a Type Expression
typeExpr :: Parser TypeExpr
typeExpr = type0
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
    variableType = VariableType <$> (flip Ident 0 <$> typeVariable)

    constructorType :: Parser TypeExpr
    constructorType = ConstructorType <$> qualType

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

--- A parser for a Qualified Type Expression | [Context =>] type0
qualTypeExpr :: Parser QualTypeExpr
qualTypeExpr = QualTypeExpr <$> context <**> typeExpr

--- A parser for a Context | {Constraint | (ConstraintList)}
context :: Parser Context
context = choice [contextList, parseSingleContext, parseNoContext]
    where
    contextList :: Parser Context
    contextList = tokenParenL **> parseList tokenComma constraint <** tokenParenR <** tokenDoubleArrow

    parseSingleContext :: Parser Context
    parseSingleContext = toList <$> constraint <** tokenDoubleArrow

    parseNoContext :: Parser Context
    parseNoContext = yield []

--- A parser for a Constraint | QualType TypeExpr
constraint :: Parser Constraint
constraint = 
    Constraint <$> qualType <**> typeExpr

--- A parser for a Qualified Type | ADD SYNTAX DESCRIPTION
qualType :: Parser QualIdent
qualType = check condition qualIdent <|> failure
    where
    condition (QualIdent (Just (ModuleIdent ids)) (Ident id _)) = all (isUpper . head) ids && (isUpper . head) id
    condition (QualIdent Nothing (Ident id _)) = (isUpper . head) id

--- A parser for a Field Declaration | IdentList :: TypeExpr
fieldDecl :: Parser FieldDecl
fieldDecl =
    FieldDecl <$>
        (map (flip Ident 0) <$> identList) <**>
        (tokenTyping **> typeExpr)

--- A parser for a Kind Expression | NOT YET IMPLEMENTED
kind :: Parser KindExpr
kind = failure

--- A parser for a Newtype
iNewtype :: Parser NewConstrDecl
iNewtype = newtypeRecord <|> newtypeSimple

--- A parser for a simple Newtype | Ident TypeExpr
newtypeSimple :: Parser NewConstrDecl
newtypeSimple = 
    parseConstructor *>= convert
    where
    convert :: (Ident, [TypeExpr]) -> Parser NewConstrDecl
    convert (i, ts) = case ts of
        [t] -> yield (NewConstrDecl i t)
        _ -> failure

--- A parser for a Record Newtype | Ident '{' FieldDecl '}'
newtypeRecord :: Parser NewConstrDecl
newtypeRecord =
    NewRecordDecl <$>
        (flip Ident 0 <$> ident) <**>
        (
            tokenCurlyBracketL **>
            parseNewField
            <** tokenCurlyBracketR
        )
    where
    parseNewField :: Parser (Ident, TypeExpr)
    parseNewField = (,) <$> (flip Ident 0 <$> ident) <**> (tokenTyping **> typeExpr)

--- A parser for a Method Declaration | Ident [Arity] '::' QualTypeExpr
methodDecl :: Parser IMethodDecl
methodDecl =
    IMethodDecl <$>
        (flip Ident 0 <$> ident) <**>
        optional arity <**>
        (tokenTyping **> qualTypeExpr)

--- A parser for an Instance Type
instanceType :: Parser InstanceType
instanceType = typeExpr

--- A parser for a Method Implementation | {Ident | '(' Op ')'} Arity
methodImpl :: Parser (Ident, Arity)--IMethodImpl
methodImpl = 
    (,) <$> (flip Ident 0 <$> (ident <|> (tokenParenL **> operator <** tokenParenR))) <**> arity

-- ################################################################
--- Helper Functions

--- Converts a value into a Singleton List
toList :: a -> [a]
toList x = [x]

-- ################################################################

--- Helper parser

--- Parses a string with enclosing parantheses
parenthesize :: Parser String -> Parser String
parenthesize p = parens <$> (tokenParenL *> p <* tokenParenR)
    where
    parens s = "(" ++ s ++ ")"

--- A parser for an Integer
parseInt :: Parser Int
parseInt = read <$> some digit 

--- A parser for a digit
digit :: Parser Char
digit = check isDigit anyChar

--- Choose the first succeeding parser from a non-empty list of parsers
choice :: [Parser a] -> Parser a
choice = foldr1 (<|>)

--- Parses a list using a parser for the seperator and a parser for the list elements
parseList :: Parser () -> Parser a -> Parser [a]
parseList psep pelem = ((:) <$> pelem <**> many (psep **> pelem)) <|> yield []

--- Tries to parse using the given parser or returns Nothing
optional :: Parser a -> Parser (Maybe a)
optional p = Just <$> p <|> yield Nothing

--- A parser for a single Pragma with a Pragma Token
parseSinglePragma :: Parser () -> Parser Ident
parseSinglePragma token = tokenPragmaL **> token **> (flip Ident 0 <$> ident) <** tokenPragmaR

parseConstructor :: Parser (Ident, [TypeExpr])
parseConstructor = 
    (,) <$>
        (flip Ident 0 <$> ident) <**>
        many (skipWhitespace *> typeExpr)

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

-- ################################################################

--- Tokens

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