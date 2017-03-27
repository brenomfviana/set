{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+                              ;
  "--".*                               ;
  program                              { \s -> Program }
  end                                  { \s -> End}
  :                                    { \s -> Colon}
  ";"                                  { \s -> SemiColon}
  natural                              { \s -> Type s}
  integer                              { \s -> Type s}
  rational                             { \s -> Type s}
  real                                 { \s -> Type s}
  universal                            { \s -> Type s}
  text                                 { \s -> Type s}
  if                                   { \s -> If}
  func                                 { \s -> Function }
  while                                { \s -> While }
  :=                                   { \s -> Assign }
  \in                                  { \s -> Belongs }
  >=                                   { \s -> GreaterOrEqual }
  >                                    { \s -> Greater }
  =                                    { \s -> Equality }
  $digit+                              { \s -> Int (read s) } -- Int or Natural?
  $alpha [$alpha $digit \_ \']*        { \s -> Id s }
  \" $alpha [$alpha $digit ! \_ \']* \"  { \s -> String s}

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Program |
  Var     |
  Begin   |
  End     |
  Colon   |
  SemiColon |
  Assign    |
  Function |
  While |
  If  |
  Then |
  Write |
  Belongs |
  Greater |
  GreaterOrEqual |
  Type String |
  Id String |
  Int Int |
  String String
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
