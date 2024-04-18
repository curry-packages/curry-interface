------------------------------------------------------------------------------
--- This library defines a parser for Curry interfaces.
------------------------------------------------------------------------------

module CurryInterface.Parser where

import CurryInterface.Types

import Prelude hiding ((*>), (<*), (<*>), (<$>), (<|>), many, empty, some)
import Data.List (init, last)
import DetParse

--- FOR TESTING
-- import System.IO.Unsafe (trace)

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
        (tokenInterface *!*> moduleIdent) <*!*>
        (tokenWhere *!*> tokenCurlyBracketL *!*> importDecls) <*>
        (decls <*!* tokenCurlyBracketR)

--- A parser for a Module Identifier.
moduleIdent :: Parser ModuleIdent
moduleIdent = ModuleIdent <$> identList

--- A parser for a list of Import Declarations.
importDecls :: Parser [IImportDecl]
importDecls = many importDecl

--- A parser for a list of Declarations.
decls :: Parser [IDecl]
decls = parseList tokenSemicolon decl

--- A parser for an Import Declaration.
importDecl :: Parser IImportDecl
importDecl = IImportDecl <$> (tokenImport *!*> moduleIdent <* tokenSemicolon <* skipSomeWs)

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
    ]

--- A parser for an Infix Declaration | Infix Arity Op
infixDecl :: Parser IDecl
infixDecl = IInfixDecl <$> iInfix <*!*> precedence <*!*> qualIdent

hidingDecl :: Parser IDecl
hidingDecl = tokenHiding *!*> (hidingDataDecl <!> hidingClassDecl)

--- A parser for a Hiding Data Declaration | hiding data QualIdent [KindExpr] TypeVariableList
hidingDataDecl :: Parser IDecl
hidingDataDecl =
    (tokenData *!*> withOptionalKind HidingDataDecl qualIdent) <*>
    (map Ident <$> typeVariableList)

--- A parser for a Hiding Class Declaration | hiding class [Context =>] QualIdent [KindExpr] TypeVariable
hidingClassDecl :: Parser IDecl
hidingClassDecl = tokenClass *!*> (case1 <|> case2)
    where
    case1 :: Parser IDecl
    case1 = (tokenParenL *> qualIdent <* skipSomeWs) *>= f
    case2 :: Parser IDecl
    case2 = (qualIdent <* skipSomeWs) *>= g
    f :: QualIdent -> Parser IDecl
    f qi =
        (HidingClassDecl [] qi <$> (Just <$> (tokenTyping *!*> kind <* tokenParenR)) <*!*> (Ident <$> typeVariable)) <!>
        ((HidingClassDecl <$> (((:) <$> (Constraint qi <$> typeExpr) <*> (tokenComma *!*> parseList tokenComma constraint)) <* tokenParenR <*!* tokenDoubleArrow <* skipSomeWs) *>=
            (flip withOptionalKind qualIdent)) <*!*> ((Ident <$> typeVariable)))
    g :: QualIdent -> Parser IDecl
    g qi =
        (ident) *>= h qi
    h :: QualIdent -> String -> Parser IDecl
    h qi i =
        (((yield (HidingClassDecl [Constraint qi (VariableType (Ident i))]) <*!* tokenDoubleArrow <* skipSomeWs) *>= (flip withOptionalKind qualIdent)) <*!*> (Ident <$> ident)) <!>
        (yield (HidingClassDecl [] qi Nothing (Ident i)))

--- A parser for a Data Declaration | data QualIdent [KindExpr] TypeVariableList = ConstrDeclList
dataDecl :: Parser IDecl
dataDecl =
    tokenData *!*> withOptionalKind IDataDecl qualIdent <*>
    (map Ident <$> typeVariableList) <*>
    ((skipSomeWs *> tokenEqual *!*> parseList (skipSomeWs *> tokenPipe) constrDecl) <!> yield []) <*?*>
    hiddenPragma

    {-
    convert <$> leftSide <*!*> (tokenEqual *!*> rightSide) <*?*> pragma
    where
    convert :: (QualIdent, Maybe KindExpr, [Ident]) -> [ConstrDecl] -> [Ident] -> IDecl
    convert (qi, mk, ids) cs ps = IDataDecl qi mk ids cs ps

    leftSide :: Parser (QualIdent, Maybe KindExpr, [Ident])
    leftSide =
        (,,) <$> (tokenData *!*> qualIdent) <*> optional (skipSomeWs *> kind) <*> (map Ident <$> typeVariableList)

    rightSide :: Parser [ConstrDecl]
    rightSide = (:) <$> constrDecl <*> many (skipSomeWs *> tokenPipe *!*> constrDecl)
    -}

--- A parser for a Newtype Declaration | newtype QualIdent [KindExpr] TypeVariableList = Newtype
newtypeDecl :: Parser IDecl
newtypeDecl =
    tokenNewtype *!*> withOptionalKind INewtypeDecl qualIdent <*>
    (map Ident <$> typeVariableList) <*!*>
    (tokenEqual *!*> iNewtype) <*?*>
    hiddenPragma

    {-
    convert <$> leftSide <*!*> (tokenEqual *!*> rightSide) <*?*> pragma
    where
    convert :: (QualIdent, Maybe KindExpr, [Ident]) -> NewConstrDecl -> [Ident] -> IDecl
    convert (qi, mk, ids) nc ps = INewtypeDecl qi mk ids nc ps

    leftSide :: Parser (QualIdent, Maybe KindExpr, [Ident])
    leftSide = (,,) <$> (tokenNewtype *!*> qualIdent) <*> optional (skipSomeWs *> kind) <*> (map Ident <$> typeVariableList)

    rightSide :: Parser NewConstrDecl
    rightSide = iNewtype
    -}

--- A parser for a Type Declaration | type QualIdent [KindExpr] TypeVariableList = TypeExpr
typeDecl :: Parser IDecl
typeDecl =
    tokenType *!*> withOptionalKind ITypeDecl qualIdent <*>
    (map Ident <$> typeVariableList) <*!*>
    (tokenEqual *!*> typeExpr)

    {-
    convert <$> leftSide <*!*> (tokenEqual *!*> rightSide)
    where
    convert :: (QualIdent, Maybe KindExpr, [Ident]) -> TypeExpr -> IDecl
    convert (qi, mk, ids) t = ITypeDecl qi mk ids t

    leftSide :: Parser (QualIdent, Maybe KindExpr, [Ident])
    leftSide = (,,) <$> (tokenType *!*> qualIdent) <*> optional (skipSomeWs *> kind) <*> (map Ident <$> typeVariableList)

    rightSide :: Parser TypeExpr
    rightSide = typeExpr
    -}

--- A parser for a Function Declaration | QualIdent [MethodPragma] Arity :: QualTypeExpr
functionDecl :: Parser IDecl
functionDecl =
    IFunctionDecl <$>
        qualIdent <*!*>
        optional (skipSomeWs *> methodPragma) <*>
        arity <*!*>
        (tokenTyping *!*> qualTypeExpr)

--- A parser for a Class Declaration | class [Context =>] QualIdent [KindExpr] TypeVariable \{ MethodList \} [Pragma]
classDecl :: Parser IDecl
classDecl = tokenClass *!*> (case1 <|> case2) <*!*> (tokenCurlyBracketL *!*> parseList tokenSemicolon methodDecl <*?* tokenCurlyBracketR) <*> hiddenPragma
    where
    case1 :: Parser ([IMethodDecl] -> [Ident] -> IDecl)
    case1 = (tokenParenL *> qualIdent <* skipSomeWs) *>= f
    case2 :: Parser ([IMethodDecl] -> [Ident] -> IDecl)
    case2 = (qualIdent <* skipSomeWs) *>= g
    f :: QualIdent -> Parser ([IMethodDecl] -> [Ident] -> IDecl)
    f qi =
        (IClassDecl [] qi <$> (Just <$> (tokenTyping *!*> kind <* tokenParenR)) <*!*> (Ident <$> typeVariable)) <!>
        ((IClassDecl <$> (((:) <$> (Constraint qi <$> typeExpr) <*> (tokenComma *!*> parseList tokenComma constraint)) <* tokenParenR <*!* tokenDoubleArrow <* skipSomeWs) *>=
            (flip withOptionalKind qualIdent)) <*!*> ((Ident <$> typeVariable)))
    g :: QualIdent -> Parser ([IMethodDecl] -> [Ident] -> IDecl)
    g qi =
        (ident) *>= h qi
    h :: QualIdent -> String -> Parser ([IMethodDecl] -> [Ident] -> IDecl)
    h qi i =
        (((yield (IClassDecl [Constraint qi (VariableType (Ident i))]) <*!* tokenDoubleArrow <* skipSomeWs) *>= (flip withOptionalKind qualIdent)) <*!*> (Ident <$> ident)) <!>
        (yield (IClassDecl [] qi Nothing (Ident i)))

--- A parser for an Instance Declaration | instance [Context =>] QualIdent InstanceType \{ MethodImplList \} [ModulePragma]
instanceDecl :: Parser IDecl
instanceDecl =
    convert IInstanceDecl <$>
        (tokenInstance *!*> (case1 <|> case2)) <*!*>
        (tokenCurlyBracketL *!*> parseList tokenSemicolon methodImpl <*!* tokenCurlyBracketR) <*>
        (optional (skipSomeWs *> modulePragma))
    where
    case1 :: Parser (Context, QualIdent, InstanceType)
    case1 = (,,) <$> contextList <*!*> qualIdent <*!*> instanceType

    case2 :: Parser (Context, QualIdent, InstanceType)
    case2 = ((,) <$> qualIdent <*!*> instanceType) *>= decide

    decide :: (QualIdent, InstanceType) -> Parser (Context, QualIdent, InstanceType)
    decide (qi, it) =
        (skipSomeWs *> tokenDoubleArrow *!*> ((,,) [Constraint qi it] <$> qualIdent <*!*> instanceType)) <!>
        yield ([], qi, it)

    convert :: (Context -> QualIdent -> InstanceType -> a) -> (Context, QualIdent, InstanceType) -> a
    convert f (ctx, qi, it) = f ctx qi it

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
qualIdent = QualIdent <$> (optional (moduleIdent <* tokenDot)) <*> (Ident <$> finalIdent)
    where
    finalIdent :: Parser String
    finalIdent = tokenParenL *> operator <* tokenParenR <!> operator <!> ident

--- A parser for a List of Identifiers | Ident [. IdentList]
identList :: Parser [String]
identList = (:) <$> ident <*> (many (tokenDot *> ident)) <!> yield []

--- A parser for an Identifier (not operator)
ident :: Parser String
ident = (:) <$> check isAlpha anyChar <*> many (check condition anyChar)
    where
    condition c = isDigit c || isAlpha c || c == '_' || c == '\''

--- A parser for an Operator
operator :: Parser String
operator = tokenBacktick *> ident <* tokenBacktick <!> check stringCondition (some (check charCondition anyChar))
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
typeVariableList = many (skipSomeWs *> typeVariable)

--- A parser for a Hidden Pragma | ADD SYNTAX DESCRIPTION
hiddenPragma :: Parser [Ident]
hiddenPragma = (tokenPragmaL *!*> word "HIDING" *!*> (map Ident <$> parseList tokenComma ident) <*!* tokenPragmaR) <!> yield []

--- A parser for a Method Pragma | ADD SYNTAX DESCRIPTION
methodPragma :: Parser Ident
methodPragma = tokenPragmaL *!*> word "METHOD" *!*> (Ident <$> ident) <*!* tokenPragmaR

--- A parser for a Module Pragma | ADD SYNTAX DESCRIPTION
modulePragma :: Parser ModuleIdent
modulePragma = tokenPragmaL *!*> word "MODULE" *!*> moduleIdent <*!* tokenPragmaR

--- A parser for a Constructor Declaration
-- Constr ::= Ident TypeVariableList
--          | Ident '{' fieldList '}'
--          | TypeExpr Op TypeExpr
constrDecl :: Parser ConstrDecl
constrDecl = (Ident <$> ident *>= decide1) <|> constrDeclOp
    where
    decide1 :: Ident -> Parser ConstrDecl
    --decide1 i = case1 i <|> case2 i
    decide1 i = (skipSomeWs *> (case1 i <!> case2 i)) <!> decide2 i []

    case1 :: Ident -> Parser ConstrDecl
    --case1 i = RecordDecl i <$> (skipSomeWs *> tokenCurlyBracketL *!*> parseList tokenComma fieldDecl <*!* tokenCurlyBracketR)
    case1 i = RecordDecl i <$> (tokenCurlyBracketL *!*> parseList tokenComma fieldDecl <*!* tokenCurlyBracketR)

    case2 :: Ident -> Parser ConstrDecl
    --case2 i = many (skipSomeWs *> typeExpr) *>= decide2 i
    case2 i = (:) <$> typeExpr <*> many (skipSomeWs *> typeExpr) *>= decide2 i

    decide2 :: Ident -> [TypeExpr] -> Parser ConstrDecl
    decide2 i ts =
        (ConOpDecl (foldl1 ApplyType (ConstructorType (identToQualIdent i):ts)) <$> (Ident <$> operator) <*!*> typeExpr) <!>
        yield (ConstrDecl i ts)

--- A parser for a simple Constructor Declaration | Ident TypeExprList
constrDeclSimple :: Parser ConstrDecl
constrDeclSimple =
    (uncurry ConstrDecl) <$> parseConstructor

--- A parser for an Operator Constructor Declaration | TypeExpr Op TypeExpr
constrDeclOp :: Parser ConstrDecl
constrDeclOp = ConOpDecl <$> typeExpr <*!*> (Ident <$> operator) <*!*> typeExpr

--- A parser for a Record Constructor Declaration | Ident \{ FieldDeclList \}
constrDeclRecord :: Parser ConstrDecl
constrDeclRecord = 
    RecordDecl <$>
        (Ident <$> ident) <*!*>
        (
            tokenCurlyBracketL *!*> 
            parseList tokenComma fieldDecl
            <*!* tokenCurlyBracketR
        )

--- A parser for a Type Expression
typeExpr :: Parser TypeExpr
typeExpr = type0

-- type0 ::= type1 ['->' type0]
type0 :: Parser TypeExpr
type0 = convert <$> type1 <*> (optional (skipSomeWs *> tokenArrow *!*> type0))
    where
    convert t1 Nothing   = t1
    convert t1 (Just t0) = ArrowType t1 t0

-- type1 ::= [type1] type2
type1 :: Parser TypeExpr
type1 = foldl1 ApplyType <$> some (skipWhitespace *> type2)

-- type2 ::= identType | parenType | bracketType
type2 :: Parser TypeExpr
type2 = parenType <!> bracketType <!> identType

identType :: Parser TypeExpr
identType = variableType <|> constructorType

variableType :: Parser TypeExpr
variableType = VariableType <$> (Ident <$> typeVariable)

constructorType :: Parser TypeExpr
constructorType = ConstructorType <$> qualType

-- parenType ::= '(' tupleType ')' | '(' arrowType ')'
parenType :: Parser TypeExpr
parenType = tokenParenL *> (arrowType <!> tupleType) <* tokenParenR

arrowType :: Parser TypeExpr
arrowType = word "->" *> yield (ConstructorType (QualIdent Nothing (Ident "->")))

-- tupleType ::= type0
--            |  type0 ',' type0 { ',' type0 }
--            |  
tupleType :: Parser TypeExpr
tupleType = convert <$> parseList tokenComma type0
    where
    convert ts = case ts of
        [t] -> ParenType t
        _   -> TupleType ts

-- bracketType ::= '[' type0 ']'
bracketType :: Parser TypeExpr
--bracketType = ListType <$> ((toList <$> (tokenBracketL *> type0 <* tokenBracketR)) <|> (word "[]" *> yield []))
bracketType = ListType <$> (tokenBracketL *> (toList <$> type0 <!> yield []) <* tokenBracketR)

--                   Prelude.Int -> Prelude.Int -> Prelude.Int;
-- Prelude.Show a => a           -> [Prelude.Char]            ;
--- A parser for a Qualified Type Expression | [Context =>] type0
qualTypeExpr :: Parser QualTypeExpr
qualTypeExpr = singleOrNoConstraint <!> multipleOrNoConstraints
    where
    singleOrNoConstraint :: Parser QualTypeExpr
    singleOrNoConstraint =
        (qualIdent *>= decide1) <!>
        (tokenBracketL *> (ListType <$> (toList <$> type0 <!> yield [])) <* tokenBracketR *>= decide8)

    -- starts with QualIdent, not yet sure if one constraint or not
    decide1 :: QualIdent -> Parser QualTypeExpr
    decide1 qi = 
        (skipSomeWs *>
            (
                (tokenArrow *!*> type0 *>= decide2 qi) <!>
                (type0 *>= decide3 qi)
            )
        ) <!>
        (yield (QualTypeExpr [] (constructorOrVariable qi)))

    -- starts with no constraints but with ArrowType
    decide2 :: QualIdent -> TypeExpr -> Parser QualTypeExpr
    decide2 qi t = 
        yield $ QualTypeExpr [] (ArrowType (constructorOrVariable qi) t)

    -- starts with QualIdent and TypeExpr
    decide3 :: QualIdent -> TypeExpr -> Parser QualTypeExpr
    decide3 qi t = let t' = addPrefix (constructorOrVariable qi) t in
        (QualTypeExpr [Constraint qi t] <$> (skipSomeWs *> tokenDoubleArrow *!*> type0)) <!>
        (yield (QualTypeExpr [] t'))

    multipleOrNoConstraints :: Parser QualTypeExpr
    multipleOrNoConstraints =
        tokenParenL *> (
            (
                tokenParenR *> (
                    (skipSomeWs *> tokenArrow *!*> ((QualTypeExpr [] . ArrowType (TupleType [])) <$> type0)) <!>
                    (yield (QualTypeExpr [] (TupleType [])))
                )
            ) <!>
            (qualIdent *>= decide4) <!>
            (((type0 *>= (\t -> (yield (ParenType t) <* tokenParenR) <!> ((TupleType . (t:)) <$> (tokenComma *!*> parseList tokenComma type0 <* tokenParenR))))) *>= decide8)
        )
    
    -- starts with QualIdent, all cases still possible
    decide4 :: QualIdent -> Parser QualTypeExpr
    decide4 qi = 
        (
            skipSomeWs *> (
                (type0 *>= decide5 qi) <!>
                (tokenArrow *!*> type0 *>= decide6 qi)
            )
        ) <!>
        (tokenComma *!*> decide7 qi)

    -- starts with QualIdent and TypeExpr, all cases still possible
    decide5 :: QualIdent -> TypeExpr -> Parser QualTypeExpr
    decide5 qi t = let t' = addPrefix (constructorOrVariable qi) t in
        --(tokenParenR *> decide8 t') <!>
        case t of
            ArrowType _ _ -> ((tokenParenR *> yield t') <!> (finishTupleType (tokenComma *> skipSomeWs) [t'])) *>= decide8
            ApplyType _ _ -> finishTupleType (tokenComma *> skipSomeWs) [t'] *>= decide8
            _ -> tokenComma *!*> decide9 [(qi, t)]

    -- starts with no context, starts with an ArrowType or a TupleType starting with an ArrowType
    decide6 :: QualIdent -> TypeExpr -> Parser QualTypeExpr
    decide6 qi t = let t' = ArrowType (constructorOrVariable qi) t in
        (tokenParenR *> decide8 t') <!>
        (finishTupleType (tokenComma *> skipSomeWs) [t'] *>= decide8)

    -- starts with no context, starts with tuple type, tuple starts with ConstructorType or VariableType
    decide7 :: QualIdent -> Parser QualTypeExpr
    decide7 qi = 
        finishTupleType empty [constructorOrVariable qi] *>= decide8

    -- starts with no context, first part of type expression parsed, rest remains to be parsed
    decide8 :: TypeExpr -> Parser QualTypeExpr
    decide8 t = 
        (
            skipSomeWs *> (
                (type0 *>= (\t' -> yield (QualTypeExpr [] (ApplyType t t')))) <!>
                (tokenArrow *!*> type0 *>= (\t' -> yield (QualTypeExpr [] (ArrowType t t'))))
            )
        ) <!>
        (yield (QualTypeExpr [] t))

    -- starts with a comma-seperated list, not yet clear if context or not
    decide9 :: [(QualIdent, TypeExpr)] -> Parser QualTypeExpr
    decide9 acc = let
            ts = map (\(qi, t) -> addPrefix (constructorOrVariable qi) t) acc'
            acc' = reverse acc
        in
            (qualIdent *>= decide10 acc) <!>
            (finishTupleType empty ts *>= decide8)
            {-
            (tokenParenR *> (
                skipSomeWs *> tokenDoubleArrow *!*> (QualTypeExpr constraints <$> type0) <|>
                decide8 ts
            ))
            -}

    -- starts with a comma-seperated list, current element starts with QualIdent, not yet clear if context or not
    decide10 :: [(QualIdent, TypeExpr)] -> QualIdent -> Parser QualTypeExpr
    decide10 acc qi = let
            acc' = reverse acc
            t' = constructorOrVariable qi
            ts = map (\(qi', t) -> addPrefix (constructorOrVariable qi') t) acc'
        in
            (finishTupleType (tokenComma *> skipSomeWs) (ts ++ [t']) *>= decide8) <!>
            (
                skipSomeWs *> (
                    ((tokenArrow *!*> type0) *>= (\t -> decide12 (ts ++ [ArrowType t' t]))) <!>
                    (type0 *>= decide11 acc qi)
                )
            ) <!>
            (tokenParenR *> decide8 (TupleType (ts ++ [t'])))
    
    -- starts with a comma-seperated list, current element starts with QualIdent and TypeExpr, not yet clear if context or not
    decide11 :: [(QualIdent, TypeExpr)] -> QualIdent -> TypeExpr -> Parser QualTypeExpr
    decide11 acc qi t = let
            acc' = reverse acc
            t' = addPrefix (constructorOrVariable qi) t
            ts = map (\(qi', t'') -> addPrefix (constructorOrVariable qi') t'') acc'
            ts' = ts ++ [t']
            constraints = map (uncurry Constraint) acc'
            constraints' = constraints ++ [Constraint qi t]
        in
            case t of
                ArrowType _ _ -> finishTupleType (tokenComma *> skipSomeWs) ts' *>= decide8
                ApplyType _ _ -> finishTupleType (tokenComma *> skipSomeWs) ts' *>= decide8
                _ ->
                    tokenComma *!*> decide9 ((qi, t):acc) <!>
                    (
                        tokenParenR *> (
                            (QualTypeExpr constraints' <$> (skipSomeWs *> tokenDoubleArrow *!*> type0)) <|>
                            (decide8 (TupleType ts))
                        )
                    )
    
    -- starts with TupleType, tuple not yet finished to be parsed
    decide12 :: [TypeExpr] -> Parser QualTypeExpr
    decide12 ts = 
        (tokenParenR *> decide8 (TupleType ts)) <!>
        (finishTupleType (tokenComma *> skipSomeWs) ts *>= decide8)
    
    addPrefix :: TypeExpr -> TypeExpr -> TypeExpr
    addPrefix p t = case t of
        ApplyType t1 t2 -> ApplyType (addPrefix p t1) t2
        ArrowType t1 t2 -> ArrowType (addPrefix p t1) t2
        _ -> ApplyType p t
    
    constructorOrVariable :: QualIdent -> TypeExpr
    constructorOrVariable qi = case qi of
        QualIdent Nothing i -> if isVariable i then VariableType i else ConstructorType qi
        _ -> ConstructorType qi
    
    isVariable :: Ident -> Bool
    isVariable (Ident s) = all isLower s
    
    finishTupleType :: Parser () -> [TypeExpr] -> Parser TypeExpr
    finishTupleType p ts = p *> ((TupleType . (ts ++)) <$> (parseList tokenComma type0 <* tokenParenR))
{-
qualTypeExpr = (QualTypeExpr <$> contextList <*!*> typeExpr) <|> (qualType *>= decide1) <|> ((QualTypeExpr []) <$> typeExpr)
    where
    decide1 :: QualIdent -> Parser QualTypeExpr
    decide1 qi = case1 qi <|> case2 qi

    -- QualTypeExpr TypeVariable ... (maybe context)
    case1 :: QualIdent -> Parser QualTypeExpr
    case1 qi = skipSomeWs *> (Ident <$> typeVariable) *>= decide2 qi

    -- QualTypeExpr ... (NOT context)
    case2 :: QualIdent -> Parser QualTypeExpr
    case2 qi = QualTypeExpr [] <$> arrowOrApply (ConstructorType qi)

    -- QualTypeExpr TypeVariable ... (maybe context)
    decide2 :: QualIdent -> Ident -> Parser QualTypeExpr
    decide2 qi i = (case3 qi i) <|> (case4 qi i)

    -- QualTypeExpr TypeVariable '=>' ... (context)
    case3 :: QualIdent -> Ident -> Parser QualTypeExpr
    case3 qi i = QualTypeExpr [Constraint qi (VariableType i)] <$> (skipSomeWs *> tokenDoubleArrow *!*> typeExpr)

    -- QualTypeExpr TypeVariable ... (no context)
    case4 :: QualIdent -> Ident -> Parser QualTypeExpr
    case4 qi i = QualTypeExpr [] <$> arrowOrApply (ApplyType (ConstructorType qi) (VariableType i))

    arrowOrApply :: TypeExpr -> Parser TypeExpr
    arrowOrApply t = (ArrowType t <$> (skipSomeWs *> tokenArrow *!*> type0)) <|> (foldl1 ApplyType <$> ((t:) <$> many (skipSomeWs *> type2)))
-}
--- A parser for a Context | {Constraint | (ConstraintList)}
context :: Parser Context
context = choice [contextList, parseSingleContext, parseNoContext]
    where
    parseSingleContext :: Parser Context
    parseSingleContext = toList <$> constraint <*!* tokenDoubleArrow

    parseNoContext :: Parser Context
    parseNoContext = yield []

--- A parser for a Constraint | QualType TypeExpr
constraint :: Parser Constraint
constraint = 
    Constraint <$> qualType <*!*> typeExpr

--- A parser for a Qualified Type | ADD SYNTAX DESCRIPTION
qualType :: Parser QualIdent
qualType = check condition qualIdent
    where
    condition (QualIdent (Just (ModuleIdent ids)) (Ident id)) = all (isUpper . head) ids && (isUpper . head) id
    condition (QualIdent Nothing (Ident id)) = (isUpper . head) id

--- A parser for a Field Declaration | IdentList :: TypeExpr
fieldDecl :: Parser FieldDecl
fieldDecl =
    FieldDecl <$>
        (map Ident <$> identList) <*!*>
        (tokenTyping *!*> typeExpr)

--- A parser for a Kind Expression | NOT YET IMPLEMENTED
kind :: Parser KindExpr
kind = kind0

kind0 :: Parser KindExpr
kind0 = convert <$> kind1 <*> (optional (skipSomeWs *> tokenArrow *!*> kind0))
    where
    convert k1 Nothing   = k1
    convert k1 (Just k0) = ArrowKind k1 k0

kind1 :: Parser KindExpr
kind1 = tokenStar *> yield Star <!> tokenParenL *> kind0 <* tokenParenR

--- A parser for a Newtype
iNewtype :: Parser NewConstrDecl
--iNewtype = newtypeRecord <|> newtypeSimple
iNewtype = (Ident <$> ident <* skipSomeWs) *>= decide1
    where
    decide1 :: Ident -> Parser NewConstrDecl
    decide1 i =
        (NewConstrDecl i <$> typeExpr) <!>
        (NewRecordDecl i <$> (
            tokenCurlyBracketL *!*> parseNewField <*!* tokenCurlyBracketR
        ))
    
    parseNewField :: Parser (Ident, TypeExpr)
    parseNewField = (,) <$> (Ident <$> ident) <*!*> (tokenTyping *!*> typeExpr)

{-
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
        (Ident <$> ident) <*!*>
        (
            tokenCurlyBracketL *!*>
            parseNewField
            <*!* tokenCurlyBracketR
        )
    where
    parseNewField :: Parser (Ident, TypeExpr)
    parseNewField = (,) <$> (Ident <$> ident) <*!*> (tokenTyping *!*> typeExpr)
-}

--- A parser for a Method Declaration | Ident [Arity] '::' QualTypeExpr
methodDecl :: Parser IMethodDecl
methodDecl =
    IMethodDecl <$>
        (Ident <$> (ident <!> (tokenParenL *> operator <* tokenParenR))) <*!*>
        optional (arity <* skipSomeWs) <*>
        (tokenTyping *!*> qualTypeExpr)

--- A parser for an Instance Type
instanceType :: Parser InstanceType
instanceType = typeExpr

--- A parser for a Method Implementation | {Ident | '(' Op ')'} Arity
methodImpl :: Parser (Ident, Arity)--IMethodImpl
methodImpl = 
    (,) <$> (Ident <$> (ident <!> (tokenParenL *> operator <* tokenParenR))) <*!*> arity

-- ################################################################
--- Helper Functions

--- Converts a value into a Singleton List
toList :: a -> [a]
toList x = [x]

identToQualIdent :: Ident -> QualIdent
identToQualIdent i = QualIdent Nothing i

-- ################################################################

--- Helper parser

contextList :: Parser Context
contextList = tokenParenL *> parseList tokenComma constraint <* tokenParenR <*!* tokenDoubleArrow 

qualIdentWithContext :: Parser (Either Context (QualIdent, Maybe KindExpr, Ident))
qualIdentWithContext =
    (Left <$> contextList <* skipSomeWs) <!>
    ((,,) <$> qualIdent <*> optional (skipSomeWs *> kind) <*> (Ident <$> (skipSomeWs *> typeVariable)) *>= decide)
    where 
    decide :: (QualIdent, Maybe KindExpr, Ident) -> Parser (Either Context (QualIdent, Maybe KindExpr, Ident))
    decide (qi, mk, tv) = case mk of
        Just _ -> yield (Right (qi, mk, tv))
        Nothing -> (skipSomeWs *> tokenDoubleArrow *!*> yield (Left [Constraint qi (VariableType tv)])) <!> yield (Right (qi, mk, tv))

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
parseList psep pelem = ((:) <$> pelem <*> many (psep *!*> pelem)) <!> yield []

--- Tries to parse using the given parser or returns Nothing
optional :: Parser a -> Parser (Maybe a)
optional p = Just <$> p <|> yield Nothing

--- A parser for a single Pragma with a Pragma Token
parseSinglePragma :: Parser () -> Parser Ident
parseSinglePragma token = tokenPragmaL *!*> token *!*> (Ident <$> ident) <*!* tokenPragmaR

parseConstructor :: Parser (Ident, [TypeExpr])
parseConstructor = 
    (,) <$>
        (Ident <$> ident) <*>
        many (skipSomeWs *> typeExpr)

withOptionalKind :: (a -> Maybe KindExpr -> b) -> Parser a -> Parser b
withOptionalKind c p = withKind <|> withoutKind
    where
    withKind =
        c <$> (tokenParenL *> p) <*!*> (tokenTyping *!*> (Just <$> kind) <* tokenParenR)
    
    withoutKind = c <$> p <*> yield Nothing
    

--- Debug function to fail with an error message of which function is not yet implemented.
missing :: String -> Parser a
missing name = (\_ -> error (name ++ " not yet implemented"))

infixl 4 <*?*>, <*?*, *?*>, <*!*>, <*!*, *!*>

(<*?*>) :: Parser (a -> b) -> Parser a -> Parser b
pa <*?*> pb = (pa <* skipManyWs) <*> pb

(<*?*) :: Parser a -> Parser b -> Parser a
pa <*?* pb = const <$> pa <*?*> pb

(*?*>) :: Parser a -> Parser b -> Parser b
pa *?*> pb = flip const <$> pa <*?*> pb

(<*!*>) :: Parser (a -> b) -> Parser a -> Parser b
pa <*!*> pb = (pa <* skipSomeWs) <*> pb

(<*!*) :: Parser a -> Parser b -> Parser a
pa <*!* pb = const <$> pa <*!*> pb

(*!*>) :: Parser a -> Parser b -> Parser b
pa *!*> pb = flip const <$> pa <*!*> pb

ws :: Parser ()
ws = check isSpace anyChar *> empty

skipManyWs :: Parser ()
skipManyWs = many ws *> empty

skipSomeWs :: Parser ()
skipSomeWs = some ws *> empty

skipWhitespace :: Parser ()
skipWhitespace = skipManyWs

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
    , tokenPragmaModule
    ]

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

tokenBacktick :: Parser ()
tokenBacktick = char '`'

tokenStar :: Parser ()
tokenStar = char '*'
