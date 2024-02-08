module CurryInterface.Pretty where

import CurryInterface.Types

import Text.Pretty

data Options = Options

defaultOptions :: Options
defaultOptions = Options

prettyInterface :: Options -> Interface -> Doc
prettyInterface options (Interface mident decls1 decls2) =
    string "interface" <+> prettyModuleIdent options mident <+> string "where" <+>
    lbrace <> linebreak <> ((vsep . punctuate semi) (map (prettyImportDecl options) decls1 ++ map (prettyDecl options) decls2))

prettyModuleIdent :: Options -> ModuleIdent -> Doc
prettyModuleIdent options (ModuleIdent ids) =
    hcat (punctuate dot (map string ids))

prettyImportDecl :: Options -> IImportDecl -> Doc
prettyImportDecl options (IImportDecl mident) =
    string "import" <+> prettyModuleIdent options mident

prettyDecl :: Options -> IDecl -> Doc
prettyDecl options (IInfixDecl inf prec qualId) =
    prettyInfix options inf <+> prettyPrecedence options prec <+> prettyQualIdent options qualId
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
    equals <+> prettyType options texp
prettyDecl options (IFunctionDecl qualId prag ari qualTExp) =
    prettyQualIdent options qualId <+> prettyMaybe (prettyPragma options) prag <+> prettyArity options ari <+> doubleColon <+> prettyQualType options qualTExp
prettyDecl options (HidingClassDecl ctx qualId mkind id) =
    string "hiding class" <+> prettyContext options ctx <+> prettyWithOptionalKind options qualId mkind <+> prettyTypeVariable options id
prettyDecl options (IClassDecl ctx qualId mkind id mDecls pragmas) =
    string "class" <+> prettyContext options ctx <+> prettyWithOptionalKind options qualId mkind <+> prettyTypeVariable options id <+>
    equals <+> prettyMethodDecls options mDecls <+> prettyPragmas options pragmas
prettyDecl options (IInstanceDecl ctx qualId itype mImpls mIdent) =
    string "instance" <+> prettyContext options ctx <+> prettyQualIdent options qualId <+> prettyInstance options itype <+>
    equals <+> prettyImplementations options mImpls <+> prettyInstancePragma options mIdent

prettyArity :: Options -> Arity -> Doc
prettyArity options = int

prettyPrecedence :: Options -> Precedence -> Doc 
prettyPrecedence options = int

prettyInfix :: Options -> Infix -> Doc
prettyInfix options InfixL = string "infixl"
prettyInfix options InfixR = string "infixr"
prettyInfix options Infix  = string "infix"

prettyIdent :: Options -> Ident -> Doc
prettyIdent options (Ident id) = case id of
    "->" -> string "(->)"
    _ -> string id

prettyQualIdent :: Options -> QualIdent -> Doc
prettyQualIdent options (QualIdent Nothing id) = prettyIdent options id
prettyQualIdent options (QualIdent (Just mident) id) =
    prettyModuleIdent options mident <> dot <> prettyIdent options id

prettyWithOptionalKind :: Options -> QualIdent -> Maybe KindExpr -> Doc
prettyWithOptionalKind options qualId Nothing = prettyQualIdent options qualId
prettyWithOptionalKind options qualId (Just k) = parens (prettyQualIdent options qualId <+> doubleColon <+> prettyKindExpr options k)

prettyTypeVariable :: Options -> Ident -> Doc
prettyTypeVariable options tvar = prettyIdent options tvar

prettyTypeVariables :: Options -> [Ident] -> Doc
prettyTypeVariables options tvars = hsep (map (prettyTypeVariable options) tvars)

prettyNewConstructor :: Options -> NewConstrDecl -> Doc
prettyNewConstructor options (NewConstrDecl id t) = prettyIdent options id <+> prettyType options t
prettyNewConstructor options (NewRecordDecl id1 (id2, t)) =
    prettyIdent options id1 <+> braces (prettyIdent options id2 <+> doubleColon <+> prettyType options t)

prettyConstructor :: Options -> ConstrDecl -> Doc
prettyConstructor options (ConstrDecl id texps) = prettyIdent options id <+> hsep (map (prettyType options) texps)
prettyConstructor options (ConOpDecl t1 id t2) = prettyType options t1 <+> prettyIdent options id <+> prettyType options t2
prettyConstructor options (RecordDecl id fields) = prettyIdent options id <+> prettyFields options fields

prettyConstructors :: Options -> [ConstrDecl] -> Doc
prettyConstructors options constrs =
    (nest 2 ) (compose (\d1 d2 -> d1 $$ bar <+> d2) (map (prettyConstructor options) constrs))

prettyField :: Options -> FieldDecl -> Doc
prettyField options (FieldDecl ids t) = (hcat . punctuate dot) (map (prettyIdent options) ids) <+> doubleColon <+> prettyType options t

prettyFields :: Options -> [FieldDecl] -> Doc
prettyFields options fields = braces ((hcat . punctuate (string ", ")) (map (prettyField options) fields))

prettyInstancePragma :: Options -> Maybe ModuleIdent -> Doc
prettyInstancePragma options _ = string "INSTANCEPRAGMA"

prettyPragma :: Options -> Ident -> Doc
prettyPragma options prag = string "PRAGMA"

prettyPragmas :: Options -> [Ident] -> Doc
prettyPragmas options prags = string "PRAGMAS"

prettyType :: Options -> TypeExpr -> Doc
prettyType options (ConstructorType qualId) = prettyQualIdent options qualId
prettyType options (ApplyType texp1 texp2) = prettyType options texp1 <+> prettyType options texp2
prettyType options (VariableType i) = prettyIdent options i
prettyType options (TupleType texps) = parens ((hcat . punctuate (string ", ")) (map (prettyType options) texps))
prettyType options (ListType texps) = brackets ((hcat . punctuate (string ", ")) (map (prettyType options) texps))
prettyType options (ArrowType texp1 texp2) = prettyType options texp1 <+> rarrow <+> prettyType options texp2
prettyType options (ParenType texp) = parens (prettyType options texp)
prettyType options (ForallType ids texp) = string "FORALLTYPE"

prettyQualType :: Options -> QualTypeExpr -> Doc
prettyQualType options (QualTypeExpr ctx texp) = prettyContext options ctx <+> prettyType options texp

prettyConstraint :: Options -> Constraint -> Doc
prettyConstraint options (Constraint qualId texp) = prettyQualIdent options qualId <+> prettyType options texp

prettyContext :: Options -> Context -> Doc
prettyContext options ctx = case ctx of
    [] -> Text.Pretty.empty
    [constr] -> prettyConstraint options constr <+> string "=>"
    _ -> braces ((hcat . punctuate (string ", ")) (map (prettyConstraint options) ctx)) <+> doubleArrow

prettyMethodDecl :: Options -> IMethodDecl -> Doc
prettyMethodDecl options (IMethodDecl id mari qualTExp) =
    prettyIdent options id <+> prettyMaybe (prettyArity options) mari <+> prettyQualType options qualTExp

prettyMethodDecls :: Options -> [IMethodDecl] -> Doc
prettyMethodDecls options mDecls = case mDecls of
    [] -> lbrace <$$> rbrace
    _  -> lbrace <$$> (nest 4 . indent 4) ((vsep . punctuate semi) (map (prettyMethodDecl options) mDecls)) <$$> rbrace

prettyInstance :: Options -> InstanceType -> Doc
prettyInstance options i = prettyType options i

prettyImplementation :: Options -> IMethodImpl -> Doc
prettyImplementation options (id, ari) = prettyIdent options id <+> prettyArity options ari

prettyImplementations :: Options -> [IMethodImpl] -> Doc
prettyImplementations options mImpls =
    braces ((hcat . punctuate (string "; ")) (map (prettyImplementation options) mImpls))

prettyKindExpr :: Options -> KindExpr -> Doc
prettyKindExpr options Star = string "*"
prettyKindExpr options (ArrowKind k1 k2) = prettyKindExpr options k1 <+> rarrow <+> prettyKindExpr options k2

--- HELPER FUNCTIONS
prettyMaybe :: (a -> Doc) -> Maybe a -> Doc
prettyMaybe _ Nothing = Text.Pretty.empty
prettyMaybe p (Just x) = p x