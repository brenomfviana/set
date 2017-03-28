{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-z]   -- alphabetic characters
$alphaB = [A-Z]   -- big alphabetic characters

tokens :-

  $white+                              ;
  "--".*                               ;
  program                              { \s -> Program }
  :                                    { \s -> Colon}
  ";"                                  { \s -> SemiColon}
  ","                                  { \s -> Comma}
  natural                              { \s -> Type s}
  integer                              { \s -> Type s}
  rational                             { \s -> Type s}
  real                                 { \s -> Type s}
  universal                            { \s -> Type s}
  text                                 { \s -> Type s}
  int                                  { \s -> Type s}
  float                                { \s -> Type s}
  string                               { \s -> Type s}
  if                                   { \s -> If}
  endif                                { \s -> End_If}
  else                                 { \s -> Else}
  endelse                              { \s -> End_Els}
  func                                 { \s -> Function }
  endfunc                              { \s -> End_Function}
  while                                { \s -> While }
  endwhile                             { \s -> End_While}
  end                                  { \s -> End}
  :=                                   { \s -> Assign }
  "\in"                                { \s -> Belongs }
  "\cap"                               { \s -> Intersection }
  "\cup"                               { \s -> Union }
  "\equiv"                             { \s -> Equivalence }
  "\subset"                            { \s -> Subset }
  "\stcomp"                            { \s -> Complement }
  "\emptyset"                          { \s -> Empty_Set }
  "{"                                  { \s -> Open_Bracket }
  "}"                                  { \s -> Close_Bracket }
  "*"                                  { \s -> Multiplication }
  "/"                                  { \s -> Division }
  "+"                                  { \s -> Addition }
  "-"                                  { \s -> Subtraction }
  >=                                   { \s -> GreaterOrEqual }
  "<="                                 { \s -> SmallerOrEqual }
  >                                    { \s -> Greater }
  "<"                                  { \s -> Smaller }
  !                                    { \s -> Denial }
  =                                    { \s -> Equality }
  print                                { \s -> Print }
  $digit+                                   { \s -> Int } -- Int or Natural?
  $digit+.$digit+                           { \s -> Float } -- Float or Real?
  $alpha [$alpha $digit \_ \']*             { \s -> Id s }
  $alphaB [$alphaB $digit \_ \']*           { \s -> Set_Id s }
  \" $alpha [$alpha $digit ! \_ \']* \"     { \s -> String s}
  \" $alphaB [$alphaB $digit ! \_ \']* \"   { \s -> String s}
  \" $alpha [$alphaB $digit ! \_ \']* \"    { \s -> String s}
  \" $alphaB [$alpha $digit ! \_ \']* \"    { \s -> String s}

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Program        |
  End            |
  Colon          |
  SemiColon      |
  Comma          |
  Assign         |
  Function       |
  End_Function   |
  While          |
  End_While      |
  Belongs        |
  Equivalence    |
  Subset         |
  Complement     |
  If             |
  End_If         |
  Else           |
  End_Els        |
  Greater        |
  GreaterOrEqual |
  Smaller        |
  SmallerOrEqual |
  Denial         |
  Equality       |
  Type String    |
  Set_Id String  |
  Id String      |
  Int            |
  Float          |
  Empty_Set      |
  Intersection   |
  Union          |
  Multiplication |
  Division       |
  Addition       |
  Subtraction    |
  Open_Bracket   |
  Close_Bracket  |
  Print          |
  String String
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
