module IsambardParser where

import IsambardScanner
import qualified Data.HashMap.Strict as HashMap

type Operator = String

type VarIdent = String

type TypeIdent = String

type SymbolTable = HashMap.HashMap VarIdent SymData

type TypeTable = HashMap.HashMap TypeIdent TypeData

data Expr
    = UnaryExpr Operator Expr
    | BinaryExpr Expr Operator Expr

data Callable = Callable
      { isProc :: Bool
      , params :: [(VarIdent, TypeIdent)]
      }


