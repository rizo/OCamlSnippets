
type binary_operator =
  | OpAssign
  | OpOr
  | OpAnd
  | OpEq | OpNEq
  | OpLT | OpLTE | OpGT | OpGTE
  | OpAdd | OpSub
  | OpMul | OpDiv | OpMod

type unary_operator = OpNot | OpPreInc | OpPreDec | OpPostInc | OpPostDec | OpReference | OpDereference | OpPlus | OpMinus


type variable_qualifier = 
  | QReference of variable_qualifier
  | QPointer   of variable_qualifier
  | QVariable

module type SyntaxType = sig
  type location
  type instruction_annotation
  type expression_annotation
  type name
end

module Syntax = functor (T : SyntaxType) -> struct
  open T

  type variable_declaration = name * variable_qualifier * name 

  type function_declaration = variable_declaration * variable_declaration list

  type expression  = expression' * expression_annotation * location
  and  expression' =
       | EInteger    of int32
       | ENull
       | EThis
       | EVariable   of name
       | EBinary     of binary_operator * expression * expression
       | EUnary      of unary_operator * expression
       | EField      of expression * name
       | ECall       of name * expression list
       | EMethodCall of expression * name * expression list
       | ENew        of name * expression list

  let expression_raw (l, _, _) = l
  let expression_annotation (_, l, _) = l
  let expression_location (_, _, l) = l

  type instruction  = instruction' * instruction_annotation * location
  and  instruction' =
       | IExpression         of expression
       | IVariable           of variable_declaration
       | IVariableAssign     of variable_declaration * expression
       | IVariableConstruct  of variable_declaration * name * expression list (* We must check if it is a construction or a function call during the first traversal *)
       | IIf                 of expression * instruction * instruction
       | IWhile              of expression * instruction
       | IFor                of expression list * expression * expression list * instruction
       | IBlock              of instruction list
       | ICoutInt            of expression
       | ICoutString         of string
       | IReturn             of expression
       | IReturnVoid

  let instruction_raw (l, _, _) = l
  let instruction_annotation (_, l, _) = l
  let instruction_location (_, _, l) = l

  type member  = member' * location
  and  member' =
       | MVariable of variable_declaration
       | MFunction of function_declaration * bool

  type declaration  = declaration' * location
  and  declaration' =
       | DClass    of name * name list * member list
       | DVariable of variable_declaration
       | DFunction of function_declaration * instruction list

  type program = declaration list
end

module SourceSyntaxType = struct
  type location               = Lexing.position * Lexing.position
  type instruction_annotation = unit
  type expression_annotation  = unit
  type name                   = Name of string | ScopedName of string * string
end

module SourceSyntax = Syntax(SourceSyntaxType)

let dummy_location = (Lexing.dummy_pos, Lexing.dummy_pos)