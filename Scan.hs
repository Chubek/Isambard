module Scanner where

import System.IO

import Data.Char (isAlpha, isAlphaNum)
import Data.List (find)
import Data.Maybe (fromJust)

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
    | Label
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
    deriving (Show, Eq)

data Delimiter
    = LBrace
    | RBrace
    | LParen
    | RParen
    | LBrack
    | RBrack
    deriving (Show, Eq)

data Punctuation
    = Semicolon
    | Comma
    | Colon
    | Period
    | Atsign
    | Caret
    deriving (Show, Eq)

data Operator
    = Plus
    | Minus
    | Times
    | Modulo
    | Divide
    | Power
    | Increase
    | Decrease
    | Greater
    | GreaterEqual
    | Lesser
    | LesserEqual
    | Equals
    | NotEquals
    | Reference
    | AddressOf
    | Assign
    | BitwiseAnd
    | BitwiseOr
    | BitwiseXor
    | LogicalAnd
    | LogicalOr

data Lexeme
    = CharString String
    | Identifier Ident
    | Operator Operator
    | Integer Int
    | Rational Float
    | Keyword Keyword
    | Label Int
    | Delimiter Delimiter
    | Punctuation Punctuation
    | Comment String
    deriving (Show, Eq)


scanIsambard :: String -> [Lexeme]
scanIsambard [] = []
scanIsambard (c:cs)
  | isSpace c = scanIsambard cs
  | isDigit c =
     let (numStr, rest) = span (\x -> isDigit x || x == '.') (c:cs)
     in if '.' `elem` numStr
     	then Rational (read numStr) : scanIsambard rest
	else Integer (read numStr) : scanIsambard rest
  | isAlpha c =
     let (idStr, rest) = span isIdChar (c:cs)
     in case classifyIdentifier idStr of
       Just keyword -> Keyword keyword : scanIsambard rest
       Nothing -> Identifier idStr : scanIsambard rest
  | c `elem` "{}[]()" = Delimiter (charToDelimiter c) : scanIsambard cs
  | c `elem` ";,.:@^" = 
      if (head cs) `eq` '='
      then Operator Assign : scanIsambard (tail cs)
      else Punctuation (charToPunctuation c) : scanIsambard cs
  | c `elem` "=:+-*/^&|$@"
      let (opStr, rest) = span (\x -> x `elem` "=:+-*/^&|$@") (c:cs)
      in Operator (strToOperator opStr) : scanIsambard rest
  | c == '{' =
    let (comment, rest) = span (/= '}') cs
    in Comment comment : scanIsambard (drop 1 rest)
  | otherwise = error $ "Unexpected character: " ++ [c]


classifyIdentifier :: String -> Maybe Keyword
classifyIdentifier ident =
  let lowercaseIdent = map toLower ident
  in find (\kw -> map toLower (show kw) == lowercaseIdent) allKeywords

allKeywords :: [Keyword]
allKeywords = [ And, Array, Begin, Case, Const, Div, Do, Downto
              , Else, End, File, For, Function, Goto, If, In
              , Label, Mod, Nil, Not, Of, Or, Packed, Procedure
              , Program, Record, Repeat, Set, Then, To, Type, Until
              , Var, While, With
              ]

isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c == '_'

charToDelimiter :: Char -> Delimiter
charToDelimiter '{' = LBrace
charToDelimiter '}' = RBrace
charToDelimiter '(' = LParen
charToDelimiter ')' = RParen
charToDelimiter '[' = LBrack
charToDelimiter ']' = RBrack
charToDelimiter _   = error "Not a delimiter"

charToPunctuation :: Char -> Punctuation
charToPunctuation ';' = Semicolon
charToPunctuation ',' = Comma
charToPunctuation ':' = Colon
charToPunctuation '.' = Period
charToPunctuation '@' = Atsign
charToPunctuation '^' = Caret
charToPunctuation _   = error "Not a punctuation"

strToOperator ::= String -> Operator
strToOperator "+" = Plus
strToOperator "-" = Minus
strToOperator "*" = Times
strToOperator "/" = Divide
strToOperator "%" = Modulo
strToOperator "**" = Power
strToOperator "++" = Increase
strToOperator "--" = Decrease
strToOperator "&" = BitwiseAnd
strToOperator "|" = BitwiseOr
strToOperator "$" = BitwiseXor
strToOperator "&&" = LogicalAnd
strToOperator "||" = LogicalOr
strToOperator "@" = AddressOf
strToOperator "^" = Reference
strToOperator "<" = Lesser
strToOperator "<=" = LesserEqual
strToOperator ">" = Greater
strToOperator ">=" = GreaterEqual
strToOperator "==" = Equals
strToOperator "<>" = NotEquals
strToOperatpr ":=" = Assign

