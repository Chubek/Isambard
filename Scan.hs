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

data Delimiter
    = LBrace
    | RBrace
    | LParen
    | RParen
    | LBrack
    | RBrack

data Punctuation
    = Semicolon
    | Comma
    | Colon
    | Period
    | Atsign
    | Caret

data Lexeme
    = CharString String
    | Identifier Ident (Maybe Punctuation)
    | Integer Int
    | Rational Float
    | Keyword Keyword
    | Label Int
    | Delimiter Delimiter
    | Punctuation Punctuation
    | Comment String


