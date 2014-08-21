
open Minicpp_syntax
open Minicpp_lexer
open Minicpp_parser
open Minicpp_typecheck
open Lexing
open Printf

let print_position outx lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  fprintf outx "File \"%s\", line %d, characters %d-%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) (pos.pos_cnum - pos.pos_bol + 1)

let parse lexbuf =
  try Minicpp_parser.program Minicpp_lexer.read lexbuf with
  | Minicpp_lexer.Error msg ->
    fprintf stderr "%a:\n%s\n" print_position lexbuf msg;
    exit 1
  | Minicpp_parser.Error ->
    fprintf stderr "%a:\nsyntax error\n" print_position lexbuf;
    exit 1
  | Minicpp_parserstate.ParseError msg ->
    fprintf stderr "%a:\n%s\n" print_position lexbuf msg;
    exit 1

let filename = ref ""
let parse_only = ref false

let argument_spec =
  [ ("--parse-only", Arg.Set parse_only, "Parse only")
  ]

let do_parse_only () =
  let file = open_in !filename in
  let lexbuf = Lexing.from_channel file in
  lexbuf.lex_start_p <- {lexbuf.lex_start_p with pos_fname = !filename};
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = !filename};
  let _ = parse lexbuf in
  ()

let _ =
  Arg.parse
    argument_spec
    (fun s -> filename := s)
    "MiniCpp";

  match !parse_only with
    | true   -> do_parse_only ()
    | _      -> fprintf stderr "Invalid flag combinaison !\n"