------------------------------------------------------------------------------
--- Definition of data types to represent interfaces of Curry modules.
---
--- These definitions are adapted from the Haskell definition contained
--- in the implementation of the Curry front end, see
--- <https://git.ps.informatik.uni-kiel.de/curry/curry-frontend/-/blob/master/src/Curry/Syntax/Type.hs>
------------------------------------------------------------------------------

module CurryInterface.Types where

--- Interface declarations are restricted to type declarations and signatures.
--- Note that an interface function declaration additionaly contains the
--- function arity (= number of parameters) in order to generate
--- correct FlatCurry function applications.
data Interface = Interface {- ModuleIdent [IImportDecl] [IDecl]
 deriving (Eq, Read, Show)

--- Interface import declaration
data IImportDecl = IImportDecl Position ModuleIdent
 deriving (Eq, Read, Show)

--- Arity of a function
type Arity = Int

--- Interface declaration
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

--- Class methods
data IMethodDecl = IMethodDecl Position Ident (Maybe Arity) QualTypeExpr
 deriving (Eq, Read, Show)

--- Class method implementations
type IMethodImpl = (Ident, Arity)

--- Kind expressions
data KindExpr
  = Star
  | ArrowKind KindExpr KindExpr
    deriving (Eq, Read, Show)
-}

-- ... and much more

-----------------------------------------------------------------------
