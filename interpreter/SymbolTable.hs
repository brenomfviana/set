-- Symbol Table
-- Version: 01/06/2017
module SymbolTable where

-- Imports
import Lexer

-- Insert a Token in symbol table
symtableInsert :: (Token, Token) -> [(Token, Token)] -> [(Token, Token)]
symtableInsert symbol [] = [symbol]
symtableInsert symbol symtable = symtable ++ [symbol]

-- Updates a Token in symbol table
symtableUpdate :: (Token, Token) -> [(Token, Token)] -> [(Token, Token)]
symtableUpdate _ [] = fail "Variable not found!"
symtableUpdate (Id id1 p1, v1) ((Id id2 p2, v2):t) =
    if id1 == id2 then (Id id1 p2, v1) : t
    else (Id id2 p2, v2) : symtableUpdate (Id id1 p1, v1) t

-- Remove a Token of symbol table
symtableRemove :: (Token, Token) -> [(Token, Token)] -> [(Token, Token)]
symtableRemove _ [] = fail "Variable not found!"
symtableRemove (id1, v1) ((id2, v2):t) =
    if id1 == id2 then t
    else (id2, v2) : symtableRemove (id1, v1) t
