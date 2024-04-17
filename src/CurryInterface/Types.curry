------------------------------------------------------------------------------
--- Definition of data types to represent interfaces of Curry modules.
---
--- These definitions are adapted from the Haskell definition contained
--- in the implementation of the Curry front end, see
--- <https://git.ps.informatik.uni-kiel.de/curry/curry-frontend/-/blob/master/src/Curry/Syntax/Type.hs>
------------------------------------------------------------------------------

module CurryInterface.Types where

import Data.Function (on)

--- Interface declarations are restricted to type declarations and signatures.
--- Note that an interface function declaration additionaly contains the
--- function arity (= number of parameters) in order to generate
--- correct FlatCurry function applications.
data Interface = Interface ModuleIdent [IImportDecl] [IDecl]
 deriving (Eq, Read, Show)

--- Interface import declaration
data IImportDecl = IImportDecl ModuleIdent
 deriving (Eq, Read, Show)

--- Arity of a function
type Arity = Int

--- Operator precedence
type Precedence = Int

-- |Fixity of operators
data Infix
  = InfixL -- ^ left-associative
  | InfixR -- ^ right-associative
  | Infix  -- ^ no associativity
    deriving (Eq, Read, Show)


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
data IMethodDecl = IMethodDecl Ident (Maybe Arity) QualTypeExpr
 deriving (Eq, Read, Show)

--- Class method implementations
type IMethodImpl = (Ident, Arity)

--- Kind expressions
data KindExpr
  = Star
  | ArrowKind KindExpr KindExpr
    deriving (Eq, Read, Show)

-- ... and much more

-----------------------------------------------------------------------
-- Token types

-- |Simple identifier
{-
data Ident = Ident 
  { idName :: String
  , idUnique :: Int
  } deriving (Read, Show)

instance Eq Ident where
  Ident m i == Ident n j = (m, i) == (n, j)

instance Ord Ident where
  Ident m i `compare` Ident n j = (m, i) `compare` (n, j)
-}

data Ident = Ident
  { idName :: String
  } deriving (Read, Show)

instance Eq Ident where
  Ident m == Ident n = m == n

instance Ord Ident where
  Ident m `compare` Ident n = m `compare` n

-- | Module identifier
data ModuleIdent = ModuleIdent 
  { midQualifiers :: [String]
  } deriving (Read, Show)

instance Eq ModuleIdent where
  (==) = (==) `on` midQualifiers

instance Ord ModuleIdent where
  compare = compare `on` midQualifiers

-- |Qualified identifier
data QualIdent = QualIdent 
  { qidModule :: Maybe ModuleIdent
  , qidIdent  :: Ident
  } deriving (Read, Show)

instance Eq QualIdent where
  QualIdent m i == QualIdent n j = (m, i) == (n, j)

instance Ord QualIdent where
  QualIdent m i `compare` QualIdent n j = (m, i) `compare` (n, j)

-- |Constructor declaration for algebraic data types
data ConstrDecl
  = ConstrDecl Ident [TypeExpr]
  | ConOpDecl TypeExpr Ident TypeExpr
  | RecordDecl Ident [FieldDecl]
    deriving (Eq, Read, Show)

-- |Constructor declaration for renaming types (newtypes)
data NewConstrDecl
  = NewConstrDecl Ident TypeExpr
  | NewRecordDecl Ident (Ident, TypeExpr)
    deriving (Eq, Read, Show)

-- |Declaration for labelled fields
data FieldDecl = FieldDecl [Ident] TypeExpr
  deriving (Eq, Read, Show)


-- |Type expressions
data TypeExpr
  = ConstructorType QualIdent
  | ApplyType       TypeExpr TypeExpr
  | VariableType    Ident
  | TupleType       [TypeExpr]
  | ListType        [TypeExpr]
  | ArrowType       TypeExpr TypeExpr
  | ParenType       TypeExpr
  | ForallType      [Ident] TypeExpr
    deriving (Eq, Read, Show)

-- |Qualified type expressions
data QualTypeExpr = QualTypeExpr Context TypeExpr
    deriving (Eq, Read, Show)


type Context = [Constraint]

data Constraint = Constraint QualIdent TypeExpr
    deriving (Eq, Read, Show)

type InstanceType = TypeExpr


-----------------------------------------------------------------------
