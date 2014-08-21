{
open Lexing
open Minicpp_parser
open Minicpp_parserstate

exception Error of string

module StringSet = Set.Make(String)

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

let digit = ['0'-'9']
let hexDigit = ['0'-'9' 'a'-'f' 'A'-'F']

let int = digit+

let string = '\"' '\"'

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
  | whitespace            { read lexbuf }
  | newline               { next_line lexbuf; read lexbuf }
  | "!"                   { NOT }
  | "++"                  { INC }
  | "--"                  { DEC }
  | "&&"                  { AND }
  | "||"                  { OR }
  | "::"                  { DOUBLECOLON }
  | '&'                   { AMPERSAND }
  | '*'                   { MULT }
  | '+'                   { PLUS }
  | '-'                   { MINUS }
  | '/'                   { DIV }
  | '%'                   { MOD }
  | ">="                  { GTE }
  | "<="                  { LTE }
  | '>'                   { GT }
  | '<'                   { LT }
  | "!="                  { NEQ }
  | "=="                  { EQ }
  | '='                   { ASSIGN }
  | "std::cout"           { STDCOUT }
  | "#include <iostream>" { INCLUDE }
  | "<<"                  { SHIFT }
  | '.'                   { DOT }
  | "->"                  { ARROW }
  | ','                   { COMMA }
  | ';'                   { SEMICOLON }
  | ':'                   { COLON }
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | '{'                   { LBRACE }
  | '}'                   { RBRACE }
  | "class"               { CLASS }
  | "else"                { ELSE }
  | "false"               { FALSE }
  | "for"                 { FOR }
  | "if"                  { IF }
  | "int"                 { INT }
  | "new"                 { NEW }
  | "NULL"                { NULL }
  | "public"              { PUBLIC }
  | "return"              { RETURN }
  | "this"                { THIS }
  | "true"                { TRUE }
  | "virtual"             { VIRTUAL }
  | "void"                { VOID }
  | "while"               { WHILE }
  | int                   { INTEGER (Int32.of_string (Lexing.lexeme lexbuf)) }
  | string                { STRING (Lexing.lexeme lexbuf) }
  | identifier            { let id = Lexing.lexeme lexbuf in
                              if StringSet.mem id !parser_state
                                then TYPEIDENTIFIER (Lexing.lexeme lexbuf)
                                else IDENTIFIER (Lexing.lexeme lexbuf)
                          }
  | _                     { raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof                   { EOF }