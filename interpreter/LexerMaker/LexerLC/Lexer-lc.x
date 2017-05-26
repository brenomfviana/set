{
module Lexer where

import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = 0-9        -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+                              ;
  "--".*                               ;
  \" [^\" \\]* \"                      { \p s -> String s (getLC p)}
  program                              { \p s -> Program (getLC p)}
  :                                    { \p s -> Colon (getLC p)}
  ";"                                  { \p s -> SemiColon (getLC p)}
  ","                                  { \p s -> Comma (getLC p)}
  natural                              { \p s -> Type s (getLC p)}
  integer                              { \p s -> Type s (getLC p)}
  rational                             { \p s -> Type s (getLC p)}
  real                                 { \p s -> Type s (getLC p)}
  universal                            { \p s -> Type s (getLC p)}
  text                                 { \p s -> Type s (getLC p)}
  int                                  { \p s -> Type s (getLC p)}
  float                                { \p s -> Type s (getLC p)}
  string                               { \p s -> Type s (getLC p)}
  "set["                               { \p s -> Set_of (getLC p)}
  "]"                                  { \p s -> End_Set_of (getLC p)}
  if                                   { \p s -> If (getLC p)}
  endif                                { \p s -> End_If (getLC p)}
  else                                 { \p s -> Else (getLC p)}
  elseif                               { \p s -> Else_If (getLC p)}
  endelse                              { \p s -> End_Els (getLC p)}
  func                                 { \p s -> Function (getLC p)}
  endfunc                              { \p s -> End_Function (getLC p)}
  while                                { \p s -> While (getLC p)}
  endwhile                             { \p s -> End_While (getLC p)}
  end                                  { \p s -> End (getLC p)}
  typedef                              { \p s -> Typedef (getLC p)}
  :=                                   { \p s -> Assign (getLC p)}
  "\in"                                { \p s -> Belongs (getLC p)}
  "\cap"                               { \p s -> Intersection (getLC p)}
  "\cup"                               { \p s -> Union (getLC p)}
  "\subset"                            { \p s -> Subset (getLC p)}
  "\stcomp"                            { \p s -> Complement (getLC p)}
  "\emptyset"                          { \p s -> Empty_Set (getLC p)}
  "{"                                  { \p s -> Open_Bracket (getLC p)}
  "}"                                  { \p s -> Close_Bracket (getLC p)}
  "("                                  { \p s -> Open_Parentheses (getLC p)}
  ")"                                  { \p s -> Close_Parentheses (getLC p)}
  "*"                                  { \p s -> Multiplication (getLC p)}
  "/"                                  { \p s -> Division (getLC p)}
  "+"                                  { \p s -> Addition (getLC p)}
  "-"                                  { \p s -> Subtraction (getLC p)}
  >=                                   { \p s -> GreaterOrEqual (getLC p)}
  "<="                                 { \p s -> SmallerOrEqual (getLC p)}
  >                                    { \p s -> Greater (getLC p)}
  "<"                                  { \p s -> Smaller (getLC p)}
  !                                    { \p s -> Denial (getLC p)}
  =                                    { \p s -> Equality (getLC p)}
  print                                { \p s -> Print (getLC p)}
  $digit+                              { \p s -> Int (read s) (getLC p)}
  $digit+.$digit+                      { \p s -> Float (read s) (getLC p)}
  $alpha [$alpha $digit \_ \']*        { \p s -> Id s (getLC p)}

{
-- Token Position
getLC (AlexPn _ l c) = (l, c)
--data L a = L { getPos :: AlexPosn, unPos :: a } deriving (Eq,Show)

-- Each action has type :: String -> Token
-- The token type:
data Token =
  Program           |
  End               |
  Colon             |
  SemiColon         |
  Comma             |
  Assign            |
  Function          |
  End_Function      |
  While             |
  End_While         |
  Belongs           |
  Subset            |
  Complement        |
  Set_of            |
  End_Set_of        |
  If                |
  End_If            |
  Else              |
  Else_If           |
  End_Els           |
  Typedef           |
  Greater           |
  GreaterOrEqual    |
  Smaller           |
  SmallerOrEqual (Int, Int) |
  Denial            |
  Equality          |
  Type String       |
  Id String         |
  Int Int           |
  Float Float       |
  Empty_Set         |
  Intersection      |
  Union             |
  Multiplication    |
  Division          |
  Addition          |
  Subtraction       |
  Open_Bracket      |
  Close_Bracket     |
  Open_Parentheses  |
  Close_Parentheses |
  Print             |
  String String
  deriving (Eq,Show)

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                   s <- hGetContents fh;
                   return (alexScanTokens s)}
}
