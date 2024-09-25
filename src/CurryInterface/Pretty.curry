------------------------------------------------------------------------------
--- This library provides a pretty-printer for Curry interfaces.
---
--- @version September 2024
------------------------------------------------------------------------------

module CurryInterface.Pretty where

import Prelude hiding ( empty )
import Data.List      ( intersperse )
import Data.Maybe     ( isNothing )
import Text.Pretty

import CurryInterface.Types

--- Options to influence the pretty printing of Curry interfaces.
data Options = Options
  { optModule       :: String  -- name of current module
  , optInstances    :: [IDecl] -- locally defined class instances
  , optQualify      :: Bool    -- show identifiers with module qualifier?
  , optWithString   :: Bool    -- show type `[Char]` as `String`?
  , optWithArity    :: Bool    -- show arity of operations?
  , optWithHiding   :: Bool    -- show `hiding` information?
  , optWithInstance :: Bool    -- show detailed `instance` information?
  , optWithImports  :: Bool    -- show also information about imported entities?
  , optIndent       :: Int     -- the number of columns for indention
  , optHelp         :: Bool    -- show help (used in main tool)
  }

--- The default options for pretty printing: show everything
defaultOptions :: Options
defaultOptions = Options "" [] True True True True True True 2 False

--- pretty-print a Curry interface
ppInterface :: Options -> Interface -> Doc
ppInterface options (Interface mident decls1 decls2) =
  text "interface" <+> ppModuleIdent options mident <+> text "where" <+>
  lbrace <> linebreak <>
  ((vsep . punctuate semi) (pdecls1 ++ pdecls2)) <$$> rbrace
 where
  pdecls1 = filter (not . isEmpty) (map (ppImportDecl options) decls1)
  pdecls2 = filter (not . isEmpty) (map (ppDecl options) decls2)

--- pretty-print a ModuleIdent
ppModuleIdent :: Options -> ModuleIdent -> Doc
ppModuleIdent _ (ModuleIdent ids) =
    hcat (punctuate dot (map text ids))

--- pretty-print an import declaration
ppImportDecl :: Options -> IImportDecl -> Doc
ppImportDecl options (IImportDecl mident) =
    text "import" <+> ppModuleIdent options mident

--- pretty-print a declaration
ppDecl :: Options -> IDecl -> Doc
ppDecl opts (IInfixDecl inf prec qualId) =
  let ppid = ppQualIdent opts 0 qualId
  in ppInfix opts inf <+> ppPrecedence opts prec <+>
     if isOperator (idName (qidIdent qualId)) then ppid else bquotes ppid
  
ppDecl opts (HidingDataDecl qualId mkind tvars)
  | optWithHiding opts
  = text "hiding data" <+> ppWithOptionalKind opts qualId mkind <+>
    ppTypeVariables opts tvars
  | otherwise = empty

ppDecl opts (IDataDecl qualId mkind tvars constrs pragmas) =
  (if optWithInstance opts
     then ppdata
     else ppdata <$$>
          ppDeriving (filter (isInstanceOf qualId) (optInstances opts)))
 where
  ppdatalhs = text "data" <+> ppWithOptionalKind opts qualId mkind <+>
              ppTypeVariables opts tvars

  ppdata = nest (optIndent opts)
    (case constrs of
       []   -> ppdatalhs
       c:cs -> if optWithHiding opts -- show all details --> with line breaks
                 then ppdatalhs <$$> equals <+> ppConstructors opts constrs
                 else fillSep (ppdatalhs : (equals <+> ppConstructor opts c) :
                               map (bar <+>) (map (ppConstructor opts) cs))) <>
    ppHiddenPragma opts pragmas

  ppDeriving []          = empty
  ppDeriving insts@(_:_) = hang 11 $
    text " deriving" <+>
    parensIf (length insts > 1)
      ((fillSep . punctuate (text ", ")) (map classOf insts))
   where
    classOf idecl = case idecl of
      IInstanceDecl _ qid _ _ _ -> ppQualIdent opts 0 qid
      _                         -> empty -- should not occur

ppDecl opts (INewtypeDecl qualId mkind tvars newconstr pragmas) =
  text "newtype" <+> ppWithOptionalKind opts qualId mkind <+>
  ppTypeVariables opts tvars <+> 
  equals <+> ppNewConstructor opts newconstr <> ppHiddenPragma opts pragmas
ppDecl opts (ITypeDecl qualId mkind tvars texp) =
  text "type" <+> ppWithOptionalKind opts qualId mkind <+>
  ppTypeVariables opts tvars <+>
  equals <+> ppType opts 0 texp
ppDecl opts (IFunctionDecl qualId prag ari qualTExp) =
  nest (optIndent opts) $ fillSep
    [ ppQualIdent opts 1 qualId
    , ppMaybe (\x -> space <> ppMethodPragma opts x) prag
    , if optWithArity opts then ppArity opts ari else empty
    , doubleColon, ppQualType opts qualTExp ]
ppDecl opts (HidingClassDecl ctx qualId mkind ids fdeps)
  | optWithHiding opts
  = text "hiding class" <+> ppContext opts ctx <+>
    ppWithOptionalKind opts qualId mkind <+> hsep (map (ppTypeVariable opts) ids) <+> ppFunDeps opts fdeps
  | otherwise = empty
ppDecl opts (IClassDecl ctx qualId mkind ids fdeps mDecls pragmas) =
  text "class" <+> ppContext opts ctx <+>
  ppWithOptionalKind opts qualId mkind <+> hsep (map (ppTypeVariable opts) ids) <+> ppFunDeps opts fdeps <+>
  ppMethodDecls opts mDecls <+> ppHiddenPragma opts pragmas
ppDecl opts (IInstanceDecl ctx qualId itype mImpls mIdent)
  | optWithInstance opts && (optWithImports opts || isNothing mIdent)
  = text "instance" <+> ppContext opts ctx <+> ppQualIdent opts 0 qualId <+>
    ppInstance opts itype <+>
    ppImplementations opts mImpls <>
    ppMaybe (\x -> space <> ppModulePragma opts x) mIdent
  | otherwise = empty

-- pretty-print the functional dependencies of a class declaration
ppFunDeps :: Options -> [FunDep] -> Doc
ppFunDeps opts fdeps | null fdeps = empty
                     | otherwise  = text "|" <+> sep (punctuate comma (map (ppFunDep opts) fdeps))
 where
  ppFunDep :: Options -> FunDep -> Doc
  ppFunDep opts' (FunDep lhs rhs) = sep (map (ppIdent opts' 0) lhs) <+> rarrow <+> sep (map (ppIdent opts' 0) rhs)

--- pretty-print an arity
ppArity :: Options -> Arity -> Doc
ppArity _ = int

--- pretty-print a precedence
ppPrecedence :: Options -> Precedence -> Doc 
ppPrecedence _ = int

--- pretty-print an infix declaration
ppInfix :: Options -> Infix -> Doc
ppInfix _ InfixL = text "infixl"
ppInfix _ InfixR = text "infixr"
ppInfix _ Infix  = text "infix"

--- Pretty-print an Ident. If the second argument is non-zero,
--- operators (i.e., strings with special characters) will be
--- enclosed in parentheses.
ppIdent :: Options -> Int -> Ident -> Doc
ppIdent _ p (Ident id) =
  parensIf (p >= 1 && isOperator id) (text id)

--- pretty-print a QualIdent
ppQualIdent :: Options -> Int -> QualIdent -> Doc
ppQualIdent opts p (QualIdent Nothing id) = ppIdent opts p id
ppQualIdent opts p (QualIdent (Just mident) id)
  | optQualify opts = ppModuleIdent opts mident <> dot <> ppIdent opts p id
  | otherwise       = ppIdent opts p id

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
  hsep (ppIdent opts 0 id : map (ppType opts 0) texps)
ppConstructor opts (ConOpDecl t1 id t2) =
  ppType opts 0 t1 <+> ppIdent opts 0 id <+> ppType opts 0 t2
ppConstructor opts (RecordDecl id fields) =
  ppIdent opts 0 id <+> ppFields opts fields

--- pretty-print a list of constructor declarations
ppConstructors :: Options -> [ConstrDecl] -> Doc
ppConstructors opts constrs =
  compose (\d1 d2 -> d1 $$ bar <+> d2) (map (ppConstructor opts) constrs)

--- pretty-print a field declaration
ppField :: Options -> FieldDecl -> Doc
ppField opts (FieldDecl ids t) =
  (hcat . punctuate dot) (map (ppIdent opts 0) ids) <+>
  doubleColon <+> ppType opts 0 t

--- pretty-print a list of field declarations
ppFields :: Options -> [FieldDecl] -> Doc
ppFields opts fields =
  lbrace <+>
  ((hcat . punctuate (text ", ")) (map (ppField opts) fields)) <+>
  rbrace

--- pretty-print a module pragma
ppModulePragma :: Options -> ModuleIdent -> Doc
ppModulePragma opts mid = lpragma <+> text "MODULE" <+> ppModuleIdent opts mid <+> rpragma

--- pretty-print a hidden pragma
ppHiddenPragma :: Options -> [Ident] -> Doc
ppHiddenPragma opts pragmas = case pragmas of
    [] -> empty
    _  -> space <> lpragma <+> text "HIDING" <+>
          (hsep . punctuate comma) (map (ppIdent opts 0) pragmas) <+> rpragma

--- pretty-print a method pragma
ppMethodPragma :: Options -> Ident -> Doc
ppMethodPragma opts id =
  lpragma <+> text "METHOD" <+> ppIdent opts 0 id <+> rpragma

--- pretty-print a type declaration
ppType :: Options -> Int -> TypeExpr -> Doc
ppType opts _ (ConstructorType qualId) = ppQualIdent opts 1 qualId
ppType opts _ (VariableType i) = ppIdent opts 0 i
ppType opts _ (TupleType texps) =
  parens ((hcat . punctuate (text ", ")) (map (ppType opts 0) texps))
ppType opts _ (ListType texps)
  | optWithString opts &&
    (optModule opts == "Prelude" && texps == [localCharType] ||
     texps == [preludeCharType])
  = text "String"
  | otherwise
  = brackets ((hcat . punctuate (text ", ")) (map (ppType opts 0) texps))
ppType opts p (ArrowType texp1 texp2) =
  parensIf (p >= 1) (ppType opts (if isArrowType texp1 then 1 else 0) texp1 </>
                     rarrow <+> ppType opts 0 texp2)
 where
  isArrowType te = case te of ArrowType _ _ -> True
                              _             -> False
ppType opts _ (ParenType texp) = parens (ppType opts 0 texp)
ppType _ _ (ForallType _ _) = text "FORALLTYPE"
ppType opts p texp@(ApplyType texp1 texp2) = parensIf (p > 0) $
  maybe (ppType opts 1 texp1 <+> ppType opts 1 texp2)
        (\qid -> (ppQualIdent opts 1 qid <+>
                  hsep (map (ppType opts 0) (argsOfApply texp))))
        (funOfApply texp)
 where
  argsOfApply te = case te of
    ApplyType (ConstructorType _) ta -> [ta]
    ApplyType t1         t2          -> argsOfApply t1 ++ [t2]
    _                                -> [] -- should not occur

--- pretty-print a QualType
ppQualType :: Options -> QualTypeExpr -> Doc
ppQualType opts (QualTypeExpr ctx texp) =
  ppContext opts ctx <+> ppType opts 0 texp

--- pretty-print a constraint
ppConstraint :: Options -> Constraint -> Doc
ppConstraint opts (Constraint qualId ts) =
  ppQualIdent opts 0 qualId <+> hsep (map (ppType opts 0) ts)

--- pretty-print a context
ppContext :: Options -> Context -> Doc
ppContext opts ctx = case ctx of
  []       -> empty
  [constr] -> ppConstraint opts constr <+> text "=>"
  _        -> parens ((hcat . punctuate (text ", "))
                        (map (ppConstraint opts) ctx)) <+> doubleArrow

--- pretty-print a method declaration
ppMethodDecl :: Options -> IMethodDecl -> Doc
ppMethodDecl opts (IMethodDecl id mari qualTExp) =
  nest (optIndent opts) $ fillSep
    [ ppIdent opts 1 id
    , if optWithArity opts then ppMaybe (ppArity opts) mari else empty
    , doubleColon, ppQualType opts qualTExp]

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
ppInstance opts it = hsep $ map (ppType opts 1) it

--- pretty-print a method implementation
ppImplementation :: Options -> IMethodImpl -> Doc
ppImplementation opts (id, ari) =
  ppIdent opts 1 id <+>
  (if optWithArity opts then ppArity opts ari else empty)

--- pretty-print a list of method implementations
ppImplementations :: Options -> [IMethodImpl] -> Doc
ppImplementations opts mImpls = case mImpls of
  [] -> lbrace <$$> rbrace
  _  -> lbrace <$$>
        (nest (optIndent opts) . indent (optIndent opts))
           ((vsep . punctuate (text "; "))
              (map (ppImplementation opts) mImpls)) <$$> rbrace

--- pretty-print a kind expression
ppKindExpr :: Options -> Int -> KindExpr -> Doc
ppKindExpr opts p (ArrowKind k1 k2) =
  parensIf (p >= 1) (ppKindExpr opts 1 k1 <+> rarrow <+> ppKindExpr opts 0 k2)
ppKindExpr _    _ Star           = text "*"
ppKindExpr _    _ ConstraintKind = text "Constraint"

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
lpragma = text "{-#"

--- pretty-print "-#}"
rpragma :: Doc
rpragma = text "-#}"

------------------------------------------------------------------------------
-- Auxiliaries:

--- `Prelude.Char` type.
preludeCharType :: TypeExpr
preludeCharType =
  ConstructorType (QualIdent (Just (ModuleIdent ["Prelude"])) (Ident "Char"))

--- Local `Char` type.
localCharType :: TypeExpr
localCharType = ConstructorType (QualIdent Nothing (Ident "Char"))

--- Checks if an instance of some type class is defined for a given identifier.
--- 
--- This function is used to determine whether the `deriving` clause of a
--- data declaration should be shown. If the instance of the type class
--- is defined locally, it is shown; otherwise, it is not shown.
---
--- Note that this only makes sense for type classes with a single type parameter.
isInstanceOf :: QualIdent -> IDecl -> Bool
isInstanceOf qtc idecl = case idecl of
  IInstanceDecl _ _ [te] _ _  -> case te of
                                 ConstructorType qc -> qc == qtc
                                 ParenType pt -> funOfApply pt == Just qtc
                                 _            -> funOfApply te == Just qtc
  _                           -> False

funOfApply :: TypeExpr -> Maybe QualIdent
funOfApply te = case te of ApplyType (ConstructorType qc) _ -> Just qc
                           ApplyType t1                   _ -> funOfApply t1
                           _                                -> Nothing

------------------------------------------------------------------------------
