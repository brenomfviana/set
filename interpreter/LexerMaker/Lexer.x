{
module Lexer where

import System.IO
import System.IO.Unsafe
}

%wrapper "basic"

$digit = 0-9        -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+                              ;
  "--".*                               ;
  \" [^\" \\]* \"                      { \s -> String s }
  program                              { \s -> Program }
  end                                  { \s -> End }
  :                                    { \s -> Colon }
  ";"                                  { \s -> SemiColon }
  ","                                  { \s -> Comma }
  Nat                                  { \s -> Type s }
  Int                                  { \s -> Type s }
  Float                                { \s -> Type s }
  Bool                                 { \s -> Type s }
  Univ                                 { \s -> Type s }
  Text                                 { \s -> Type s }
  Pointer                              { \s -> Type s }
  "set["                               { \s -> Set_of }
  "]"                                  { \s -> End_Set_of }
  if                                   { \s -> If }
  endif                                { \s -> End_If }
  else                                 { \s -> Else }
  elseif                               { \s -> Else_If }
  func                                 { \s -> Function }
  endfunc                              { \s -> End_Function}
  proc                                 { \s -> Proc }
  endproc                              { \s -> End_Proc}
  while                                { \s -> While }
  endwhile                             { \s -> End_While }
  typedef                              { \s -> Typedef }
  :=                                   { \s -> Assign }
  "\in"                                { \s -> Belongs }
  "\cap"                               { \s -> Intersection }
  "\cup"                               { \s -> Union }
  "\subset"                            { \s -> Subset }
  "\stcomp"                            { \s -> Complement }
  "\emptyset"                          { \s -> Empty_Set }
  "{"                                  { \s -> Open_Bracket }
  "}"                                  { \s -> Close_Bracket }
  "("                                  { \s -> Open_Parentheses }
  ")"                                  { \s -> Close_Parentheses }
  "+"                                  { \s -> Addition }
  "-"                                  { \s -> Subtraction }
  "*"                                  { \s -> Multiplication }
  "/"                                  { \s -> Division }
  >                                    { \s -> Greater }
  >=                                   { \s -> GreaterOrEqual }
  "<"                                  { \s -> Smaller }
  "<="                                 { \s -> SmallerOrEqual }
  !                                    { \s -> Denial }
  =                                    { \s -> Equality }
  print                                { \s -> Print }
  input                                { \s -> Input }
  exit                                 { \s -> Exit }
  break                                { \s -> Break }
  continue                             { \s -> Continue }
  $digit+                              { \s -> Int (read s) }
  $digit+.$digit+                      { \s -> Float (read s) }
  $alpha [$alpha $digit \_ \']*        { \s -> Id s }

{

-- Each action has type :: String -> Token
-- The token type:
data Token =
  Program           |
  End               |
  Colon             |
  SemiColon         |
  Comma             |
  Id String         |
  Type String       |
--  Nat Nat           |
  Int Int           |
  Float Float         |
--  Bool Bool         |
--  Univ Univ         |
  Text String       |
--  Pointer Pointer   |
  Set_of            |
  End_Set_of        |
  If                |
  End_If            |
  Else              |
  Else_If           |
  Function          |
  End_Function      |
  Proc              |
  End_Proc          |
  While             |
  End_While         |
  Typedef           |
  Assign            |
  Belongs           |
  Intersection      |
  Union             |
  Subset            |
  Complement        |
  Empty_Set         |
  Open_Bracket      |
  Close_Bracket     |
  Open_Parentheses  |
  Close_Parentheses |
  Addition          |
  Subtraction       |
  Multiplication    |
  Division          |
  Greater           |
  GreaterOrEqual    |
  Smaller           |
  SmallerOrEqual    |
  Denial            |
  Equality          |
  Print             |
  Input             |
  Exit              |
  Break             |
  Continue          |
  String String
  deriving (Eq,Show)

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                   s <- hGetContents fh;
                   return (alexScanTokens s)}
}
