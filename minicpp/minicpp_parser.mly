%{

  open Minicpp_syntax
  open SourceSyntax
  open Minicpp_parserstate
%}

%token          NOT
%token          AND
%token          OR
%token          INC
%token          DEC
%token          AMPERSAND
%token          PLUS
%token          MINUS
%token          MULT
%token          DIV
%token          MOD
%token          SHIFT
%token          LT
%token          LTE
%token          GT
%token          GTE
%token          EQ
%token          NEQ
%token          ASSIGN
%token          CLASS
%token          ELSE
%token          FALSE
%token          FOR
%token          IF
%token          INT
%token          NEW
%token          NULL
%token          PUBLIC
%token          RETURN
%token          THIS
%token          TRUE
%token          INCLUDE
%token          VIRTUAL
%token          VOID
%token          WHILE
%token          STDCOUT
%token          DOT
%token          ARROW
%token          COMMA
%token          DOUBLECOLON
%token          COLON
%token          SEMICOLON
%token          RPAREN
%token          LPAREN
%token          RBRACE
%token          LBRACE
%token          EOF
%token <int32>  INTEGER
%token <string> STRING
%token <string> IDENTIFIER
%token <string> TYPEIDENTIFIER

%nonassoc binary
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT LTE GT GTE
%left PLUS MINUS
%left MULT DIV MOD
%nonassoc unary
%left INC DEC

%left DOT ARROW
%nonassoc LPAREN RPAREN
%nonassoc LBRACE RBRACE

%nonassoc IF
%nonassoc ELSE

%start <Minicpp_syntax.SourceSyntax.declaration list> program
%%

program:
  | program_init
    option(INCLUDE)
    p = list(declaration)
    program_cleanup
    EOF
      { p }

program_init:
  | { parser_init () }
program_cleanup:
  | { parser_cleanup () }


// Expression 

unary_operator:
  | NOT       { OpNot }
  | INC       { OpPreInc }
  | DEC       { OpPreDec }
  | AMPERSAND { OpReference }
  | MULT      { OpDereference }
  | PLUS      { OpPlus }
  | MINUS     { OpMinus }

post_unary_operator:
  | INC       { OpPostInc }
  | DEC       { OpPostDec }

binary_operator:
  | ASSIGN    { OpAssign }
  | OR        { OpOr }
  | AND       { OpAnd }
  | EQ        { OpEq }
  | NEQ       { OpNEq }
  | LT        { OpLT }
  | LTE       { OpLTE }
  | GT        { OpGT }
  | GTE       { OpGTE }
  | PLUS      { OpAdd }
  | MINUS     { OpSub }
  | MULT      { OpMul }
  | DIV       { OpDiv }
  | MOD       { OpMod }

expression:
  | e = expression_raw
      { (e, (), ($startpos, $endpos)) }
  | e = delimited(LPAREN, expression, RPAREN)
      { e }

expression_raw:
  | i  = INTEGER    { EInteger i }
  | id = IDENTIFIER { EVariable (Minicpp_syntax.SourceSyntaxType.Name id) }
  | FALSE { EInteger (Int32.of_int 0) }
  | TRUE { EInteger (Int32.of_int 1) }
  | NULL { ENull }
  | THIS { EThis }
  | e1 = expression
    op = binary_operator
    e2 = expression
    %prec binary { EBinary (op, e1, e2) }
  | op = unary_operator
    e  = expression
    %prec unary { EUnary (op, e) }
  | e  = expression
    op = post_unary_operator
    %prec unary { EUnary (op, e) }
  | n  = IDENTIFIER
    es = delimited(LPAREN, separated_list(COMMA, expression), RPAREN)
      { ECall (Minicpp_syntax.SourceSyntaxType.Name n, es) }
  | e  = expression
    ARROW
    n  = IDENTIFIER
    es = delimited(LPAREN, separated_list(COMMA, expression), RPAREN)
      { EMethodCall ((EUnary (OpDereference, e), (), expression_location e), Minicpp_syntax.SourceSyntaxType.Name n, es) }
  | e  = expression
    DOT
    n  = IDENTIFIER
    es = delimited(LPAREN, separated_list(COMMA, expression), RPAREN)
      { EMethodCall (e, Minicpp_syntax.SourceSyntaxType.Name n, es) }
  | e = expression
    ARROW
    n = IDENTIFIER
      { EField ((EUnary (OpDereference, e), (), expression_location e), Minicpp_syntax.SourceSyntaxType.Name n) }
  | e = expression
    DOT
    n = IDENTIFIER
      { EField (e, Minicpp_syntax.SourceSyntaxType.Name n) }
  | NEW
    ty = TYPEIDENTIFIER
    es = delimited(LPAREN, separated_list(COMMA, expression), RPAREN)
      { ENew (Minicpp_syntax.SourceSyntaxType.Name ty, es) }

// Instruction

instruction:
  | i = instruction_raw
      { (i, (), ($startpos, $endpos)) }

instruction_raw:
  | IF
    c  = delimited(LPAREN, expression, RPAREN)
    i1 = instruction
    i2 = if_else
      { IIf (c, i1, i2) }
  | WHILE
    c  = delimited(LPAREN, expression, RPAREN)
    i = instruction
      { IWhile (c, i) }
  | FOR
    LPAREN
    es1 = separated_list(COMMA, expression)
    SEMICOLON
    e   = for_second
    SEMICOLON
    es2 = separated_list(COMMA, expression)
    RPAREN
    i   = instruction
      { IFor (es1, e, es2, i) }
  | STDCOUT
    es = list(cout_element)
    SEMICOLON
      { IBlock es }
  | is = block
      { IBlock is }
  | e = expression
    SEMICOLON
    { IExpression e }
  | ty = typename
    qu = qualifier
    na = IDENTIFIER
    SEMICOLON
    { IVariable (ty, qu, Minicpp_syntax.SourceSyntaxType.Name na) }
  | ty = typename
    qu = qualifier
    na = IDENTIFIER
    ASSIGN
    e = expression
    SEMICOLON
    { IVariableAssign ((ty, qu, Minicpp_syntax.SourceSyntaxType.Name na), e) }
  | ty = typename
    qu = qualifier
    na = IDENTIFIER
    ASSIGN
    co = TYPEIDENTIFIER
    es = delimited(LPAREN, separated_list(COMMA, expression), RPAREN)
    SEMICOLON
    { IVariableConstruct ((ty, qu, Minicpp_syntax.SourceSyntaxType.Name na), Minicpp_syntax.SourceSyntaxType.Name co, es) }
  | RETURN
    SEMICOLON
      { IReturnVoid }
  | RETURN
    e = expression
    SEMICOLON
      { IReturn e }

qualifier:
  | qs = list(qualifier_single)
      { List.fold_right (fun f x -> f x) qs QVariable }

qualifier_single:
  | AMPERSAND { (fun x -> QReference x) }
  | MULT      { (fun x -> QPointer x) }

typename:
  | i = TYPEIDENTIFIER { Minicpp_syntax.SourceSyntaxType.Name i }
  | INT                { Minicpp_syntax.SourceSyntaxType.Name "int" }
  | VOID               { Minicpp_syntax.SourceSyntaxType.Name "void" }

block:
  | is = delimited(LBRACE, list(instruction), RBRACE) { is }

cout_element:
  | SHIFT
    e = expression
      { (ICoutInt e, (), ($startpos, $endpos)) }
  | SHIFT
    s = STRING
      { (ICoutString s, (), ($startpos, $endpos)) }

if_else:
  | ELSE
    i = instruction
    %prec ELSE { i }
  | %prec IF   { (IBlock [], (), ($startpos, $endpos)) }

for_second:
  | e = expression
      { e }
  |   { (EInteger (Int32.of_int 1), (), ($startpos, $endpos)) }

// Declaration

declaration:
  | d = declaration_raw
      { (d, ($startpos, $endpos)) }

declaration_raw:
  | CLASS
    n  = class_name
    ss = supers
    ms = delimited(LBRACE, list(class_member), RBRACE)
    SEMICOLON
      { DClass (n, ss, ms) }
  | proto = function_prototype
    block = block
      { DFunction (proto, block) }
  | var = variable_qualifier
    SEMICOLON
      { DVariable var }

class_name:
  | n = IDENTIFIER { parser_add_type_identifier n; Minicpp_syntax.SourceSyntaxType.Name n }

supers:
  | COLON
    ss = list(preceded(PUBLIC, TYPEIDENTIFIER))
      { List.map (fun s -> Minicpp_syntax.SourceSyntaxType.Name s) ss }
  | { [] }

class_member:
  | d = class_member_raw
      { (d, ($startpos, $endpos)) }
class_member_raw:
  | proto = terminated(function_prototype, SEMICOLON)
      { MFunction (proto, false) }
  | VIRTUAL
    proto = terminated(function_prototype, SEMICOLON)
      { MFunction (proto, true) }
  | var = terminated(variable_qualifier, SEMICOLON)
      { MVariable var }

function_prototype:
  | ty = typename
    qu = qualifier
    na = function_name
    args  = delimited(LPAREN, list(variable_qualifier), RPAREN)
      { ((ty, qu, na), args) }

function_name:
  | n = IDENTIFIER
      { Minicpp_syntax.SourceSyntaxType.Name n }
  | ty = TYPEIDENTIFIER
    DOUBLECOLON
    n = IDENTIFIER
      { Minicpp_syntax.SourceSyntaxType.ScopedName (ty, n) }
  | ty = TYPEIDENTIFIER
    DOUBLECOLON
    n = TYPEIDENTIFIER
      { Minicpp_syntax.SourceSyntaxType.ScopedName (ty, n) }

variable_qualifier:
  | ty = typename
    qu = qualifier
    na = IDENTIFIER
      { (ty, qu, Minicpp_syntax.SourceSyntaxType.Name na) }