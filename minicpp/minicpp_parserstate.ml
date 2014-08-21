

exception ParseError of string
  
module StringSet = Set.Make(String)

type parser_state = StringSet.t
let parser_state = ref StringSet.empty

let parser_init () = 
  parser_state := StringSet.empty
let parser_cleanup () = 
  StringSet.iter print_string !parser_state;
  parser_state := StringSet.empty
let parser_add_type_identifier ty =
  parser_state := StringSet.add ty !parser_state