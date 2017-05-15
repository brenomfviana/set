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
  \" [^\" \\]* \"                      { \pos s -> L { getPos = pos, unPos = String s }}
  program                              { \pos s -> L { getPos = pos, unPos = Program }}
  :                                    { \pos s -> L { getPos = pos, unPos = Colon }}
  ";"                                  { \pos s -> L { getPos = pos, unPos = SemiColon }}
  ","                                  { \pos s -> L { getPos = pos, unPos = Comma }}
  natural                              { \pos s -> L { getPos = pos, unPos = Type s }}
  integer                              { \pos s -> L { getPos = pos, unPos = Type s }}
  rational                             { \pos s -> L { getPos = pos, unPos = Type s }}
  real                                 { \pos s -> L { getPos = pos, unPos = Type s }}
  universal                            { \pos s -> L { getPos = pos, unPos = Type s }}
  text                                 { \pos s -> L { getPos = pos, unPos = Type s }}
  int                                  { \pos s -> L { getPos = pos, unPos = Type s }}
  float                                { \pos s -> L { getPos = pos, unPos = Type s }}
  string                               { \pos s -> L { getPos = pos, unPos = Type s }}
  "set["                               { \pos s -> L { getPos = pos, unPos = Set_of }}
  "]"                                  { \pos s -> L { getPos = pos, unPos = End_Set_of }}
  if                                   { \pos s -> L { getPos = pos, unPos = If }}
  endif                                { \pos s -> L { getPos = pos, unPos = End_If }}
  else                                 { \pos s -> L { getPos = pos, unPos = Else }}
  elseif                               { \pos s -> L { getPos = pos, unPos = Else_If }}
  endelse                              { \pos s -> L { getPos = pos, unPos = End_Els }}
  func                                 { \pos s -> L { getPos = pos, unPos = Function }}
  endfunc                              { \pos s -> L { getPos = pos, unPos = End_Function}}
  while                                { \pos s -> L { getPos = pos, unPos = While }}
  endwhile                             { \pos s -> L { getPos = pos, unPos = End_While }}
  end                                  { \pos s -> L { getPos = pos, unPos = End }}
  typedef                              { \pos s -> L { getPos = pos, unPos = Typedef }}
  :=                                   { \pos s -> L { getPos = pos, unPos = Assign }}
  "\in"                                { \pos s -> L { getPos = pos, unPos = Belongs }}
  "\cap"                               { \pos s -> L { getPos = pos, unPos = Intersection }}
  "\cup"                               { \pos s -> L { getPos = pos, unPos = Union }}
  "\subset"                            { \pos s -> L { getPos = pos, unPos = Subset }}
  "\stcomp"                            { \pos s -> L { getPos = pos, unPos = Complement }}
  "\emptyset"                          { \pos s -> L { getPos = pos, unPos = Empty_Set }}
  "{"                                  { \pos s -> L { getPos = pos, unPos = Open_Bracket }}
  "}"                                  { \pos s -> L { getPos = pos, unPos = Close_Bracket }}
  "("                                  { \pos s -> L { getPos = pos, unPos = Open_Parentheses }}
  ")"                                  { \pos s -> L { getPos = pos, unPos = Close_Parentheses }}
  "*"                                  { \pos s -> L { getPos = pos, unPos = Multiplication }}
  "/"                                  { \pos s -> L { getPos = pos, unPos = Division }}
  "+"                                  { \pos s -> L { getPos = pos, unPos = Addition }}
  "-"                                  { \pos s -> L { getPos = pos, unPos = Subtraction }}
  >=                                   { \pos s -> L { getPos = pos, unPos = GreaterOrEqual }}
  "<="                                 { \pos s -> L { getPos = pos, unPos = SmallerOrEqual }}
  >                                    { \pos s -> L { getPos = pos, unPos = Greater }}
  "<"                                  { \pos s -> L { getPos = pos, unPos = Smaller }}
  !                                    { \pos s -> L { getPos = pos, unPos = Denial }}
  =                                    { \pos s -> L { getPos = pos, unPos = Equality }}
  print                                { \pos s -> L { getPos = pos, unPos = Print }}
  $digit+                              { \pos s -> L { getPos = pos, unPos = Int (read s) }}
  $digit+.$digit+                      { \pos s -> L { getPos = pos, unPos = Float (read s) }}
  $alpha [$alpha $digit \_ \']*        { \pos s -> L { getPos = pos, unPos = Id s }}

{
-- Token Position
data L a = L { getPos :: AlexPosn, unPos :: a } deriving (Eq,Show)

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
  SmallerOrEqual    |
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
