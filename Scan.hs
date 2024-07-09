import System.IO
import Data.Char

type Ident = String

data Keyword
    = And
    | Array
    | Begin
    | Case
    | Const
    | Div
    | Do
    | Downto
    | Else
    | End
    | File
    | For
    | Function
    | Goto
    | If
    | In
    | Lable
    | Mod
    | Nil
    | Not
    | Of
    | Or
    | Packed
    | Procedure
    | Program
    | Record
    | Repeat
    | Set
    | Then
    | To
    | Type
    | Until
    | Var
    | While
    | With

data Delim 
    = LBrace
    | RBrace
    | LParen
    | RParen
    | LBrack
    | RBrack

data Symbol
    = Semicolon
    | Comma
    | Colon
    | Period

data Token
    = CharString String
    | Identifier Ident
    | Integer Int
    | Rational Float
    | Keyword Keyword
    | Label Int
    | Delimiter Delim
    | Symbol Symbol




