module CurryInterface.Pretty where

import CurryInterface.Types

import Prelude hiding (empty)
import Text.Pretty

--- Options to influence the pretty printing of Curry interfaces.
data Options = Options
  { optQualify    :: Bool -- print identifiers with module qualifier?
  , optWithArity  :: Bool -- print arity of operations?
  , optWithHiding :: Bool -- print `hiding` information?
  , optIndent     :: Int  -- the number of columns for indention
  }

--- The default options for pretty printing: show everything
defaultOptions :: Options
defaultOptions = Options True True True 2

--- pretty-print a Curry interface
ppInterface :: Options -> Interface -> Doc
ppInterface options (Interface mident decls1 decls2) =
    string "interface" <+> ppModuleIdent options mident <+> string "where" <+>
    lbrace <> linebreak <> ((vsep . punctuate semi) (pdecls1 ++ pdecls2)) <$$> rbrace
    where
    pdecls1 = filter (not . isEmpty) (map (ppImportDecl options) decls1)
    pdecls2 = filter (not . isEmpty) (map (ppDecl options) decls2)

--- pretty-print a ModuleIdent
ppModuleIdent :: Options -> ModuleIdent -> Doc
ppModuleIdent options (ModuleIdent ids) =
    hcat (punctuate dot (map string ids))

--- pretty-print an import declaration
ppImportDecl :: Options -> IImportDecl -> Doc
ppImportDecl options (IImportDecl mident) =
    string "import" <+> ppModuleIdent options mident

--- pretty-print a declaration
ppDecl :: Options -> IDecl -> Doc
ppDecl options (IInfixDecl inf prec qualId) =
    ppInfix options inf <+> ppPrecedence options prec <+> ppQualIdent options 0 qualId
ppDecl options (HidingDataDecl qualId mkind tvars)
    | optWithHiding options = string "hiding data" <+> ppWithOptionalKind options qualId mkind <+> ppTypeVariables options tvars
    | otherwise = empty
ppDecl options (IDataDecl qualId mkind tvars constrs pragmas) =
    string "data" <+> ppWithOptionalKind options qualId mkind <+> ppTypeVariables options tvars <$$>
    (nest (optIndent options) . indent (optIndent options)) equals <+> ppConstructors options constrs <> ppHiddenPragma options pragmas
ppDecl options (INewtypeDecl qualId mkind tvars newconstr pragmas) =
    string "newtype" <+> ppWithOptionalKind options qualId mkind <+> ppTypeVariables options tvars <+> 
    equals <+> ppNewConstructor options newconstr <> ppHiddenPragma options pragmas
ppDecl options (ITypeDecl qualId mkind tvars texp) =
    string "type" <+> ppWithOptionalKind options qualId mkind <+> ppTypeVariables options tvars <+>
    equals <+> ppType options 0 texp
ppDecl options (IFunctionDecl qualId prag ari qualTExp) =
    ppQualIdent options 1 qualId <> ppMaybe (\x -> space <> ppMethodPragma options x) prag <+>
    (if optWithArity options then ppArity options ari else empty) <+> doubleColon <+> ppQualType options qualTExp
ppDecl options (HidingClassDecl ctx qualId mkind id)
    | optWithHiding options = string "hiding class" <+> ppContext options ctx <+> ppWithOptionalKind options qualId mkind <+> ppTypeVariable options id
    | otherwise = empty
ppDecl options (IClassDecl ctx qualId mkind id mDecls pragmas) =
    string "class" <+> ppContext options ctx <+> ppWithOptionalKind options qualId mkind <+> ppTypeVariable options id <+>
    ppMethodDecls options mDecls <+> ppHiddenPragma options pragmas
ppDecl options (IInstanceDecl ctx qualId itype mImpls mIdent) =
    string "instance" <+> ppContext options ctx <+> ppQualIdent options 0 qualId <+> ppInstance options itype <+>
    ppImplementations options mImpls <> ppMaybe (\x -> space <> ppModulePragma options x) mIdent

--- pretty-print an arity
ppArity :: Options -> Arity -> Doc
ppArity options = int

--- pretty-print a precedence
ppPrecedence :: Options -> Precedence -> Doc 
ppPrecedence options = int

--- pretty-print an infix declaration
ppInfix :: Options -> Infix -> Doc
ppInfix options InfixL = string "infixl"
ppInfix options InfixR = string "infixr"
ppInfix options Infix  = string "infix"

--- pretty-print an Ident
ppIdent :: Options -> Int -> Ident -> Doc
ppIdent options p (Ident id)
    | p >= 1 && isOperator id = parens t
    | otherwise = t
    where
    t = string id

--- pretty-print a QualIdent
ppQualIdent :: Options -> Int -> QualIdent -> Doc
ppQualIdent options p (QualIdent Nothing id) = ppIdent options p id
ppQualIdent options p (QualIdent (Just mident) id)
    | optQualify options = ppModuleIdent options mident <> dot <> ppIdent options p id
    | otherwise = ppIdent options p id

--- pretty-print a QualIdent with an optional kind expression
ppWithOptionalKind :: Options -> QualIdent -> Maybe KindExpr -> Doc
ppWithOptionalKind options qualId Nothing = ppQualIdent options 0 qualId
ppWithOptionalKind options qualId (Just k) = parens (ppQualIdent options 0 qualId <+> doubleColon <+> ppKindExpr options 0 k)

--- pretty-print a type variable
ppTypeVariable :: Options -> Ident -> Doc
ppTypeVariable options tvar = ppIdent options 0 tvar

--- pretty-print a list of type variables
ppTypeVariables :: Options -> [Ident] -> Doc
ppTypeVariables options tvars = hsep (map (ppTypeVariable options) tvars)

--- pretty-print a newtype constructor declaration
ppNewConstructor :: Options -> NewConstrDecl -> Doc
ppNewConstructor options (NewConstrDecl id t) = ppIdent options 0 id <+> ppType options 0 t
ppNewConstructor options (NewRecordDecl id1 (id2, t)) =
    ppIdent options 0 id1 <+> lbrace <+> (ppIdent options 0 id2 <+> doubleColon <+> ppType options 0 t) <+> rbrace

--- pretty-print a constructor declaration
ppConstructor :: Options -> ConstrDecl -> Doc
ppConstructor options (ConstrDecl id texps) = ppIdent options 0 id <+> hsep (map (ppType options 0) texps)
ppConstructor options (ConOpDecl t1 id t2) = ppType options 0 t1 <+> ppIdent options 0 id <+> ppType options 0 t2
ppConstructor options (RecordDecl id fields) = ppIdent options 0 id <+> ppFields options fields

--- pretty-print a list of constructor declarations
ppConstructors :: Options -> [ConstrDecl] -> Doc
ppConstructors options constrs =
    (nest (optIndent options) ) (compose (\d1 d2 -> d1 $$ bar <+> d2) (map (ppConstructor options) constrs))

--- pretty-print a field declaration
ppField :: Options -> FieldDecl -> Doc
ppField options (FieldDecl ids t) = (hcat . punctuate dot) (map (ppIdent options 0) ids) <+> doubleColon <+> ppType options 0 t

--- pretty-print a list of field declarations
ppFields :: Options -> [FieldDecl] -> Doc
ppFields options fields = lbrace <+> ((hcat . punctuate (string ", ")) (map (ppField options) fields)) <+> rbrace

--- pretty-print a module pragma
ppModulePragma :: Options -> ModuleIdent -> Doc
ppModulePragma options mid = lpragma <+> string "MODULE" <+> ppModuleIdent options mid <+> rpragma

--- pretty-print a hidden pragma
ppHiddenPragma :: Options -> [Ident] -> Doc
ppHiddenPragma options pragmas = case pragmas of
    [] -> empty
    _ -> space <> lpragma <+> string "HIDING" <+> (hsep . punctuate comma) (map (ppIdent options 0) pragmas) <+> rpragma

--- pretty-print a method pragma
ppMethodPragma :: Options -> Ident -> Doc
ppMethodPragma options id = lpragma <+> string "METHOD" <+> ppIdent options 0 id <+> rpragma

--- pretty-print a type declaration
ppType :: Options -> Int -> TypeExpr -> Doc
ppType options _ (ConstructorType qualId) = ppQualIdent options 0 qualId
ppType options _ (ApplyType texp1 texp2) = ppType options 0 texp1 <+> ppType options 0 texp2
ppType options _ (VariableType i) = ppIdent options 0 i
ppType options _ (TupleType texps) = parens ((hcat . punctuate (string ", ")) (map (ppType options 0) texps))
ppType options _ (ListType texps) = brackets ((hcat . punctuate (string ", ")) (map (ppType options 0) texps))
ppType options p (ArrowType texp1 texp2)
    | p >= 1 = parens t
    | otherwise = t
    where
    t = ppType options 1 texp1 <+> rarrow <+> ppType options 0 texp2
ppType options _ (ParenType texp) = parens (ppType options 0 texp)
ppType options _ (ForallType ids texp) = string "FORALLTYPE"

--- pretty-print a QualType
ppQualType :: Options -> QualTypeExpr -> Doc
ppQualType options (QualTypeExpr ctx texp) = ppContext options ctx <+> ppType options 0 texp

--- pretty-print a constraint
ppConstraint :: Options -> Constraint -> Doc
ppConstraint options (Constraint qualId texp) = ppQualIdent options 0 qualId <+> ppType options 0 texp

--- pretty-print a context
ppContext :: Options -> Context -> Doc
ppContext options ctx = case ctx of
    [] -> empty
    [constr] -> ppConstraint options constr <+> string "=>"
    _ -> parens ((hcat . punctuate (string ", ")) (map (ppConstraint options) ctx)) <+> doubleArrow

--- pretty-print a method declaration
ppMethodDecl :: Options -> IMethodDecl -> Doc
ppMethodDecl options (IMethodDecl id mari qualTExp) =
    ppIdent options 0 id <+> 
    (if optWithArity options then ppMaybe (ppArity options) mari else empty) <+> doubleColon <+> ppQualType options qualTExp

--- pretty-print a list of method declarations
ppMethodDecls :: Options -> [IMethodDecl] -> Doc
ppMethodDecls options mDecls = case mDecls of
    [] -> lbrace <$$> rbrace
    _  -> lbrace <$$> (nest (optIndent options) . indent (optIndent options)) ((vsep . punctuate semi) (map (ppMethodDecl options) mDecls)) <$$> rbrace

--- pretty-print an instance
ppInstance :: Options -> InstanceType -> Doc
ppInstance options i = ppType options 0 i

--- pretty-print a method implementation
ppImplementation :: Options -> IMethodImpl -> Doc
ppImplementation options (id, ari) =
    ppIdent options 0 id <+>
    (if optWithArity options then ppArity options ari else empty)

--- pretty-print a list of method implementations
ppImplementations :: Options -> [IMethodImpl] -> Doc
ppImplementations options mImpls = case mImpls of
    [] -> lbrace <$$> rbrace
    _ -> lbrace <$$> (nest (optIndent options) . indent (optIndent options)) ((vsep . punctuate (string "; ")) (map (ppImplementation options) mImpls)) <$$> rbrace

--- pretty-print a kind expression
ppKindExpr :: Options -> Int -> KindExpr -> Doc
ppKindExpr options _ Star = string "*"
ppKindExpr options p (ArrowKind k1 k2)
    | p >= 1 = parens t
    | otherwise = t
    where
    t = ppKindExpr options 1 k1 <+> rarrow <+> ppKindExpr options 0 k2

--- HELPER FUNCTIONS
--- pretty-print Just as normal, Nothing as empty
ppMaybe :: (a -> Doc) -> Maybe a -> Doc
ppMaybe _ Nothing = empty
ppMaybe p (Just x) = p x

--- Check if string is an operator
isOperator :: String -> Bool
isOperator = all (flip elem allowed)
    where
    allowed = "!#$%&*+./<=>?@\\^|-~:"

--- pretty-print "{-#"
lpragma :: Doc
lpragma = string "{-#"

--- pretty-print "-#}"
rpragma :: Doc
rpragma = string "-#}"