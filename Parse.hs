module IsambardParser where

import IsambardScanner
import qualified Data.HashMap.Strict as HashMap

type Type = String

data Name
    = Ordinay String
    | Referenced String
    | Redirected String
    deriving (Show, Eq)

data Term
    = Name Name
    | Integer Int
    | Rational Float
    deriving (Show, Eq)

data Operator 
    = Add
    | Sub
    | Mul
    | Mod
    | Div
    | Pow
    | Eq
    | Ne
    | Lt
    | Le
    | Gt
    | Ge
    | And
    | Or
    | Xor
    | Conj
    | Disj
    deriving (Show, Eq)

data Expr
    = Unary Operator Expr
    | Binary Expr Operator Expr
    | Call Name [Expr]
    | Basic Term
    deriving (Show, Eq)

data Callable = Callable
      { isProc :: Bool
      , params :: [(Name, Type)]
      , returns :: Type
      , body :: Statement
      }
      deriving (Show, Eq)

data Record =
    { staticPart :: [(Name, Type)]
    , variantPart :: (Name, [(Name, Expr)])
    }

data Declration
    = Variable Name Type
    | Constant Name Term
    | TypeAlias Name Type
    | Record Name Record

data Statement 
    = Expr [Expr]
    | Cond Expr Statement [(Expr, Statement)] Statement
    | While Expr Statement
    | DoWhile Statement Expr
    | RepeatStatement Statement Expr
    | For (Name, Expr, Expr) Statement
    | Assign Name Expr
    | Compound [Statement]
    | Continue
    | Break
    | Goto Name
    | Callable Callable
    | Declration Declration
    deriving (Show, Eq)


-- Symtable to keep tabs on syntactic analysis
-- Later used in semantic analysis and proof-checking
symtable :: HashMap.HashMap
symtable = HashMap.empty


parseIsambard :: [Lexeme]
