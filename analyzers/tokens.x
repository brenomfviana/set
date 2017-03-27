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
  :=                                   { \s -> Assign}
  if                                   { \s -> If}
  >=                                   { \s -> Greater}
  =                                    { \s -> Equality}
  $digit+                              { \s -> Int (read s) }
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
  If  |
  Then |
  Write |
  Greater |
  Type String |
  Id String |
  Int Int |
  String String
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}