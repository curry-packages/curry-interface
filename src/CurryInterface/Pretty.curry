module CurryInterface.Pretty where

import CurryInterface.Types

import Text.Pretty

data Options = Options

defaultOptions :: Options
defaultOptions = Options

prettyInterface :: Options -> Interface -> Doc
prettyInterface options (Interface mident decls1 decls2) =
    string "interface" <+> prettyModuleIdent options mident <+> string "where" <+>
    lbrace <> linebreak <> ((vsep . punctuate semi) (map (prettyImportDecl options) decls1 ++ map (prettyDecl options) decls2)) <$$> rbrace

prettyModuleIdent :: Options -> ModuleIdent -> Doc
prettyModuleIdent options (ModuleIdent ids) =
    hcat (punctuate dot (map string ids))

prettyImportDecl :: Options -> IImportDecl -> Doc
prettyImportDecl options (IImportDecl mident) =
    string "import" <+> prettyModuleIdent options mident

prettyDecl :: Options -> IDecl -> Doc
prettyDecl options (IInfixDecl inf prec qualId) =
    prettyInfix options inf <+> prettyPrecedence options prec <+> prettyQualIdent options 0 qualId
prettyDecl options (HidingDataDecl qualId mkind tvars) =
    string "hiding data" <+> prettyWithOptionalKind options qualId mkind <+> prettyTypeVariables options tvars
prettyDecl options (IDataDecl qualId mkind tvars constrs pragmas) =
    string "data" <+> prettyWithOptionalKind options qualId mkind <+> prettyTypeVariables options tvars <$$>
    (nest 2 . indent 2) equals <+> prettyConstructors options constrs <+> prettyPragmas options pragmas
prettyDecl options (INewtypeDecl qualId mkind tvars newconstr pragmas) =
    string "newtype" <+> prettyWithOptionalKind options qualId mkind <+> prettyTypeVariables options tvars <+> 
    equals <+> prettyNewConstructor options newconstr <+> prettyPragmas options pragmas
prettyDecl options (ITypeDecl qualId mkind tvars texp) =
    string "type" <+> prettyWithOptionalKind options qualId mkind <+> prettyTypeVariables options tvars <+>
    equals <+> prettyType options 0 texp
prettyDecl options (IFunctionDecl qualId prag ari qualTExp) =
    prettyQualIdent options 1 qualId <+> prettyMaybe (prettyPragma options) prag <+> prettyArity options ari <+> doubleColon <+> prettyQualType options qualTExp
prettyDecl options (HidingClassDecl ctx qualId mkind id) =
    string "hiding class" <+> prettyContext options ctx <+> prettyWithOptionalKind options qualId mkind <+> prettyTypeVariable options id
prettyDecl options (IClassDecl ctx qualId mkind id mDecls pragmas) =
    string "class" <+> prettyContext options ctx <+> prettyWithOptionalKind options qualId mkind <+> prettyTypeVariable options id <+>
    prettyMethodDecls options mDecls <+> prettyPragmas options pragmas
prettyDecl options (IInstanceDecl ctx qualId itype mImpls mIdent) =
    string "instance" <+> prettyContext options ctx <+> prettyQualIdent options 0 qualId <+> prettyInstance options itype <+>
    prettyImplementations options mImpls <+> prettyInstancePragma options mIdent

prettyArity :: Options -> Arity -> Doc
prettyArity options = int

prettyPrecedence :: Options -> Precedence -> Doc 
prettyPrecedence options = int

prettyInfix :: Options -> Infix -> Doc
prettyInfix options InfixL = string "infixl"
prettyInfix options InfixR = string "infixr"
prettyInfix options Infix  = string "infix"

prettyIdent :: Options -> Int -> Ident -> Doc
prettyIdent options p (Ident id)
    | p >= 1 && isOperator id = parens t
    | otherwise = t
    where
    t = string id

prettyQualIdent :: Options -> Int -> QualIdent -> Doc
prettyQualIdent options p (QualIdent Nothing id) = prettyIdent options p id
prettyQualIdent options p (QualIdent (Just mident) id) =
    prettyModuleIdent options mident <> dot <> prettyIdent options p id

prettyWithOptionalKind :: Options -> QualIdent -> Maybe KindExpr -> Doc
prettyWithOptionalKind options qualId Nothing = prettyQualIdent options 0 qualId
prettyWithOptionalKind options qualId (Just k) = parens (prettyQualIdent options 0 qualId <+> doubleColon <+> prettyKindExpr options 0 k)

prettyTypeVariable :: Options -> Ident -> Doc
prettyTypeVariable options tvar = prettyIdent options 0 tvar

prettyTypeVariables :: Options -> [Ident] -> Doc
prettyTypeVariables options tvars = hsep (map (prettyTypeVariable options) tvars)

prettyNewConstructor :: Options -> NewConstrDecl -> Doc
prettyNewConstructor options (NewConstrDecl id t) = prettyIdent options 0 id <+> prettyType options 0 t
prettyNewConstructor options (NewRecordDecl id1 (id2, t)) =
    prettyIdent options 0 id1 <+> lbrace <+> (prettyIdent options 0 id2 <+> doubleColon <+> prettyType options 0 t) <+> rbrace

prettyConstructor :: Options -> ConstrDecl -> Doc
prettyConstructor options (ConstrDecl id texps) = prettyIdent options 0 id <+> hsep (map (prettyType options 0) texps)
prettyConstructor options (ConOpDecl t1 id t2) = prettyType options 0 t1 <+> prettyIdent options 0 id <+> prettyType options 0 t2
prettyConstructor options (RecordDecl id fields) = prettyIdent options 0 id <+> prettyFields options fields

prettyConstructors :: Options -> [ConstrDecl] -> Doc
prettyConstructors options constrs =
    (nest 2 ) (compose (\d1 d2 -> d1 $$ bar <+> d2) (map (prettyConstructor options) constrs))

prettyField :: Options -> FieldDecl -> Doc
prettyField options (FieldDecl ids t) = (hcat . punctuate dot) (map (prettyIdent options 0) ids) <+> doubleColon <+> prettyType options 0 t

prettyFields :: Options -> [FieldDecl] -> Doc
prettyFields options fields = lbrace <+> ((hcat . punctuate (string ", ")) (map (prettyField options) fields)) <+> rbrace

prettyInstancePragma :: Options -> Maybe ModuleIdent -> Doc
prettyInstancePragma options _ = string "INSTANCEPRAGMA"

prettyPragma :: Options -> Ident -> Doc
prettyPragma options prag = string "PRAGMA"

prettyPragmas :: Options -> [Ident] -> Doc
prettyPragmas options prags = string "PRAGMAS"

prettyType :: Options -> Int -> TypeExpr -> Doc
prettyType options _ (ConstructorType qualId) = prettyQualIdent options 0 qualId
prettyType options _ (ApplyType texp1 texp2) = prettyType options 0 texp1 <+> prettyType options 0 texp2
prettyType options _ (VariableType i) = prettyIdent options 0 i
prettyType options _ (TupleType texps) = parens ((hcat . punctuate (string ", ")) (map (prettyType options 0) texps))
prettyType options _ (ListType texps) = brackets ((hcat . punctuate (string ", ")) (map (prettyType options 0) texps))
prettyType options p (ArrowType texp1 texp2)
    | p >= 1 = parens t
    | otherwise = t
    where
    t = prettyType options 1 texp1 <+> rarrow <+> prettyType options 0 texp2
prettyType options _ (ParenType texp) = parens (prettyType options 0 texp)
prettyType options _ (ForallType ids texp) = string "FORALLTYPE"

prettyQualType :: Options -> QualTypeExpr -> Doc
prettyQualType options (QualTypeExpr ctx texp) = prettyContext options ctx <+> prettyType options 0 texp

prettyConstraint :: Options -> Constraint -> Doc
prettyConstraint options (Constraint qualId texp) = prettyQualIdent options 0 qualId <+> prettyType options 0 texp

prettyContext :: Options -> Context -> Doc
prettyContext options ctx = case ctx of
    [] -> Text.Pretty.empty
    [constr] -> prettyConstraint options constr <+> string "=>"
    _ -> parens ((hcat . punctuate (string ", ")) (map (prettyConstraint options) ctx)) <+> doubleArrow

prettyMethodDecl :: Options -> IMethodDecl -> Doc
prettyMethodDecl options (IMethodDecl id mari qualTExp) =
    prettyIdent options 0 id <+> prettyMaybe (prettyArity options) mari <+> doubleColon <+> prettyQualType options qualTExp

prettyMethodDecls :: Options -> [IMethodDecl] -> Doc
prettyMethodDecls options mDecls = case mDecls of
    [] -> lbrace <$$> rbrace
    _  -> lbrace <$$> (nest 4 . indent 4) ((vsep . punctuate semi) (map (prettyMethodDecl options) mDecls)) <$$> rbrace

prettyInstance :: Options -> InstanceType -> Doc
prettyInstance options i = prettyType options 0 i

prettyImplementation :: Options -> IMethodImpl -> Doc
prettyImplementation options (id, ari) = prettyIdent options 0 id <+> prettyArity options ari

prettyImplementations :: Options -> [IMethodImpl] -> Doc
prettyImplementations options mImpls = case mImpls of
    [] -> lbrace <$$> rbrace
    _ -> lbrace <$$> (nest 4 . indent 4) ((vsep . punctuate (string "; ")) (map (prettyImplementation options) mImpls)) <$$> rbrace

prettyKindExpr :: Options -> Int -> KindExpr -> Doc
prettyKindExpr options _ Star = string "*"
prettyKindExpr options p (ArrowKind k1 k2)
    | p >= 1 = parens t
    | otherwise = t
    where
    t = prettyKindExpr options 1 k1 <+> rarrow <+> prettyKindExpr options 0 k2

--- HELPER FUNCTIONS
prettyMaybe :: (a -> Doc) -> Maybe a -> Doc
prettyMaybe _ Nothing = Text.Pretty.empty
prettyMaybe p (Just x) = p x

isOperator :: String -> Bool
isOperator = all (flip elem allowed)
    where
    allowed = "!#$%&*+./<=>?@\\^|-~:"