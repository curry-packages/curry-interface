module CurryInterface.Pretty where

import Prelude hiding (empty)
import Data.Maybe     ( isNothing )
import Text.Pretty

import CurryInterface.Types

--- Options to influence the pretty printing of Curry interfaces.
data Options = Options
  { optQualify      :: Bool -- print identifiers with module qualifier?
  , optWithArity    :: Bool -- print arity of operations?
  , optWithHiding   :: Bool -- print `hiding` information?
  , optWithInstance :: Bool -- print detailed `instance` information?
  , optWithImports  :: Bool -- show also information about imported entities?
  , optIndent       :: Int  -- the number of columns for indention
  , optHelp         :: Bool -- show help (used in main tool)
  }

--- The default options for pretty printing: show everything
defaultOptions :: Options
defaultOptions = Options True True True True True 2 False

--- pretty-print a Curry interface
ppInterface :: Options -> Interface -> Doc
ppInterface options (Interface mident decls1 decls2) =
  string "interface" <+> ppModuleIdent options mident <+> string "where" <+>
  lbrace <> linebreak <>
  ((vsep . punctuate semi) (pdecls1 ++ pdecls2)) <$$> rbrace
 where
  pdecls1 = filter (not . isEmpty) (map (ppImportDecl options) decls1)
  pdecls2 = filter (not . isEmpty) (map (ppDecl options) decls2)

--- pretty-print a ModuleIdent
ppModuleIdent :: Options -> ModuleIdent -> Doc
ppModuleIdent _ (ModuleIdent ids) =
    hcat (punctuate dot (map string ids))

--- pretty-print an import declaration
ppImportDecl :: Options -> IImportDecl -> Doc
ppImportDecl options (IImportDecl mident) =
    string "import" <+> ppModuleIdent options mident

--- pretty-print a declaration
ppDecl :: Options -> IDecl -> Doc
ppDecl opts (IInfixDecl inf prec qualId) =
  ppInfix opts inf <+> ppPrecedence opts prec <+> ppQualIdent opts 0 qualId
ppDecl opts (HidingDataDecl qualId mkind tvars)
  | optWithHiding opts
  = string "hiding data" <+> ppWithOptionalKind opts qualId mkind <+>
    ppTypeVariables opts tvars
  | otherwise = empty
ppDecl opts (IDataDecl qualId mkind tvars constrs pragmas) =
  string "data" <+> ppWithOptionalKind opts qualId mkind <+>
  ppTypeVariables opts tvars <$$>
  (nest (optIndent opts) . indent (optIndent opts)) equals <+>
  ppConstructors opts constrs <> ppHiddenPragma opts pragmas
ppDecl opts (INewtypeDecl qualId mkind tvars newconstr pragmas) =
  string "newtype" <+> ppWithOptionalKind opts qualId mkind <+>
  ppTypeVariables opts tvars <+> 
  equals <+> ppNewConstructor opts newconstr <> ppHiddenPragma opts pragmas
ppDecl opts (ITypeDecl qualId mkind tvars texp) =
  string "type" <+> ppWithOptionalKind opts qualId mkind <+>
  ppTypeVariables opts tvars <+>
  equals <+> ppType opts 0 texp
ppDecl opts (IFunctionDecl qualId prag ari qualTExp) =
  ppQualIdent opts 1 qualId <>
  ppMaybe (\x -> space <> ppMethodPragma opts x) prag <+>
  (if optWithArity opts then ppArity opts ari else empty) <+>
  doubleColon <+> ppQualType opts qualTExp
ppDecl opts (HidingClassDecl ctx qualId mkind id)
  | optWithHiding opts
  = string "hiding class" <+> ppContext opts ctx <+>
    ppWithOptionalKind opts qualId mkind <+> ppTypeVariable opts id
  | otherwise = empty
ppDecl opts (IClassDecl ctx qualId mkind id mDecls pragmas) =
  string "class" <+> ppContext opts ctx <+>
  ppWithOptionalKind opts qualId mkind <+> ppTypeVariable opts id <+>
  ppMethodDecls opts mDecls <+> ppHiddenPragma opts pragmas
ppDecl opts (IInstanceDecl ctx qualId itype mImpls mIdent)
  | optWithInstance opts && (optWithImports opts || isNothing mIdent)
  = string "instance" <+> ppContext opts ctx <+> ppQualIdent opts 0 qualId <+> ppInstance opts itype <+>
    ppImplementations opts mImpls <> ppMaybe (\x -> space <> ppModulePragma opts x) mIdent
  | otherwise = empty

--- pretty-print an arity
ppArity :: Options -> Arity -> Doc
ppArity _ = int

--- pretty-print a precedence
ppPrecedence :: Options -> Precedence -> Doc 
ppPrecedence _ = int

--- pretty-print an infix declaration
ppInfix :: Options -> Infix -> Doc
ppInfix _ InfixL = string "infixl"
ppInfix _ InfixR = string "infixr"
ppInfix _ Infix  = string "infix"

--- pretty-print an Ident
ppIdent :: Options -> Int -> Ident -> Doc
ppIdent _ p (Ident id)
  | p >= 1 && isOperator id = parens t
  | otherwise = t
 where
  t = string id

--- pretty-print a QualIdent
ppQualIdent :: Options -> Int -> QualIdent -> Doc
ppQualIdent opts p (QualIdent Nothing id) = ppIdent opts p id
ppQualIdent opts p (QualIdent (Just mident) id)
  | optQualify opts = ppModuleIdent opts mident <> dot <> ppIdent opts p id
  | otherwise = ppIdent opts p id

--- pretty-print a QualIdent with an optional kind expression
ppWithOptionalKind :: Options -> QualIdent -> Maybe KindExpr -> Doc
ppWithOptionalKind opts qualId Nothing = ppQualIdent opts 0 qualId
ppWithOptionalKind opts qualId (Just k) =
  parens (ppQualIdent opts 0 qualId <+> doubleColon <+> ppKindExpr opts 0 k)

--- pretty-print a type variable
ppTypeVariable :: Options -> Ident -> Doc
ppTypeVariable opts tvar = ppIdent opts 0 tvar

--- pretty-print a list of type variables
ppTypeVariables :: Options -> [Ident] -> Doc
ppTypeVariables opts tvars = hsep (map (ppTypeVariable opts) tvars)

--- pretty-print a newtype constructor declaration
ppNewConstructor :: Options -> NewConstrDecl -> Doc
ppNewConstructor opts (NewConstrDecl id t) =
  ppIdent opts 0 id <+> ppType opts 0 t
ppNewConstructor opts (NewRecordDecl id1 (id2, t)) =
  ppIdent opts 0 id1 <+>
  lbrace <+> (ppIdent opts 0 id2 <+> doubleColon <+> ppType opts 0 t) <+> rbrace

--- pretty-print a constructor declaration
ppConstructor :: Options -> ConstrDecl -> Doc
ppConstructor opts (ConstrDecl id texps) =
  ppIdent opts 0 id <+> hsep (map (ppType opts 0) texps)
ppConstructor opts (ConOpDecl t1 id t2) =
  ppType opts 0 t1 <+> ppIdent opts 0 id <+> ppType opts 0 t2
ppConstructor opts (RecordDecl id fields) =
  ppIdent opts 0 id <+> ppFields opts fields

--- pretty-print a list of constructor declarations
ppConstructors :: Options -> [ConstrDecl] -> Doc
ppConstructors opts constrs =
  nest (optIndent opts)
    (compose (\d1 d2 -> d1 $$ bar <+> d2) (map (ppConstructor opts) constrs))

--- pretty-print a field declaration
ppField :: Options -> FieldDecl -> Doc
ppField opts (FieldDecl ids t) =
  (hcat . punctuate dot) (map (ppIdent opts 0) ids) <+>
  doubleColon <+> ppType opts 0 t

--- pretty-print a list of field declarations
ppFields :: Options -> [FieldDecl] -> Doc
ppFields opts fields =
  lbrace <+>
  ((hcat . punctuate (string ", ")) (map (ppField opts) fields)) <+>
  rbrace

--- pretty-print a module pragma
ppModulePragma :: Options -> ModuleIdent -> Doc
ppModulePragma opts mid = lpragma <+> string "MODULE" <+> ppModuleIdent opts mid <+> rpragma

--- pretty-print a hidden pragma
ppHiddenPragma :: Options -> [Ident] -> Doc
ppHiddenPragma opts pragmas = case pragmas of
    [] -> empty
    _ -> space <> lpragma <+> string "HIDING" <+> (hsep . punctuate comma) (map (ppIdent opts 0) pragmas) <+> rpragma

--- pretty-print a method pragma
ppMethodPragma :: Options -> Ident -> Doc
ppMethodPragma opts id = lpragma <+> string "METHOD" <+> ppIdent opts 0 id <+> rpragma

--- pretty-print a type declaration
ppType :: Options -> Int -> TypeExpr -> Doc
ppType opts _ (ConstructorType qualId) = ppQualIdent opts 0 qualId
ppType opts _ (ApplyType texp1 texp2) =
  ppType opts 0 texp1 <+> ppType opts 0 texp2
ppType opts _ (VariableType i) = ppIdent opts 0 i
ppType opts _ (TupleType texps) =
  parens ((hcat . punctuate (string ", ")) (map (ppType opts 0) texps))
ppType opts _ (ListType texps) =
  brackets ((hcat . punctuate (string ", ")) (map (ppType opts 0) texps))
ppType opts p (ArrowType texp1 texp2)
  | p >= 1 = parens t
  | otherwise = t
 where
  t = ppType opts 1 texp1 <+> rarrow <+> ppType opts 0 texp2
ppType opts _ (ParenType texp) = parens (ppType opts 0 texp)
ppType _ _ (ForallType _ _) = string "FORALLTYPE"

--- pretty-print a QualType
ppQualType :: Options -> QualTypeExpr -> Doc
ppQualType opts (QualTypeExpr ctx texp) =
  ppContext opts ctx <+> ppType opts 0 texp

--- pretty-print a constraint
ppConstraint :: Options -> Constraint -> Doc
ppConstraint opts (Constraint qualId texp) =
  ppQualIdent opts 0 qualId <+> ppType opts 0 texp

--- pretty-print a context
ppContext :: Options -> Context -> Doc
ppContext opts ctx = case ctx of
  []       -> empty
  [constr] -> ppConstraint opts constr <+> string "=>"
  _        -> parens ((hcat . punctuate (string ", "))
                        (map (ppConstraint opts) ctx)) <+> doubleArrow

--- pretty-print a method declaration
ppMethodDecl :: Options -> IMethodDecl -> Doc
ppMethodDecl opts (IMethodDecl id mari qualTExp) =
  ppIdent opts 0 id <+> 
  (if optWithArity opts then ppMaybe (ppArity opts) mari else empty) <+>
  doubleColon <+> ppQualType opts qualTExp

--- pretty-print a list of method declarations
ppMethodDecls :: Options -> [IMethodDecl] -> Doc
ppMethodDecls opts mDecls = case mDecls of
  [] -> lbrace <$$> rbrace
  _  -> lbrace <$$>
        (nest (optIndent opts) . indent (optIndent opts))
           ((vsep . punctuate semi) (map (ppMethodDecl opts) mDecls)) <$$>
        rbrace

--- pretty-print an instance
ppInstance :: Options -> InstanceType -> Doc
ppInstance opts i = ppType opts 0 i

--- pretty-print a method implementation
ppImplementation :: Options -> IMethodImpl -> Doc
ppImplementation opts (id, ari) =
  ppIdent opts 0 id <+>
  (if optWithArity opts then ppArity opts ari else empty)

--- pretty-print a list of method implementations
ppImplementations :: Options -> [IMethodImpl] -> Doc
ppImplementations opts mImpls = case mImpls of
  [] -> lbrace <$$> rbrace
  _  -> lbrace <$$>
        (nest (optIndent opts) . indent (optIndent opts))
           ((vsep . punctuate (string "; "))
              (map (ppImplementation opts) mImpls)) <$$> rbrace

--- pretty-print a kind expression
ppKindExpr :: Options -> Int -> KindExpr -> Doc
ppKindExpr _       _ Star = string "*"
ppKindExpr opts p (ArrowKind k1 k2)
  | p >= 1 = parens t
  | otherwise = t
 where
  t = ppKindExpr opts 1 k1 <+> rarrow <+> ppKindExpr opts 0 k2

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
