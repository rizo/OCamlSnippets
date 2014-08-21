open Monad
open Monad_reader
open Monad_either
open Monad_state

open Minicpp_syntax

type name = SourceSyntaxType.name
module NameSet = Set.Make(struct
  type t = name
  let compare = Pervasives.compare
end)

module NameMap = Map.Make(struct
    type t = name
    let compare = Pervasives.compare
  end)

let namemap_to_set m   = NameMap.fold (fun k _ -> NameSet.add k) m NameSet.empty
let namemap_of_set f m = NameSet.fold (fun k -> NameMap.add k (f k)) m NameMap.empty
let namemap_inter a b = namemap_to_set @@ NameMap.merge
  (fun _ v1 v2 -> match (v1, v2) with
    | (Some v, None)    -> None
    | (None, Some v)    -> None
    | (Some v, Some v') -> Some v
    | (None, None)      -> None
  ) a b


type ttype   = Typenull | Typeint | Typevoid | Typeclass of name | Typepointer of ttype
type rtype   = ttype * bool

let type_of_name = function
  | SourceSyntaxType.Name "int" -> Typeint
  | SourceSyntaxType.Name "void" -> Typevoid
  | n -> Typeclass n

module TypecheckSyntaxType = struct
  type location               = Lexing.position * Lexing.position
  type instruction_annotation = ttype NameMap.t
  (* type, lhs? *)
  type expression_annotation  = rtype
  type name                   = SourceSyntaxType.name
end

let expression_type_r (_, ty, _) = ty
let expression_type (_, (ty, _), _) = ty
let expression_lhs  (_, (_, b), _)  = b

module TypecheckSyntax = Syntax(TypecheckSyntaxType)

module TypeListMap = Map.Make(struct
  type t = rtype list
  let rec compare = function
    | [] -> (function
      | [] -> 0
      | _ -> -1
    )
    | (x::xs) -> (function
      | [] -> 1
      | (y::ys) ->
          let c = Pervasives.compare x y in
            if c = 0
              then compare xs ys
              else c
    )
end)

type class_unique_member =
  | FLocal of name
  | FParent of name * class_unique_member

type tclass  =
  { class_supers    : NameSet.t
  ; class_functions : ((rtype * NameSet.t option * (TypecheckSyntax.instruction list) option) TypeListMap.t) NameMap.t
  ; class_members   : ttype NameMap.t
  ; class_unique_members   : (class_unique_member * ttype) NameMap.t
  }

type reader_context =
  { context_bindings            : ttype NameMap.t
  ; context_current_class       : name option
  ; context_current_return_type : rtype option
  }

type state_context =
  { context_functions : ((rtype * TypecheckSyntax.instruction list) TypeListMap.t) NameMap.t
  ; context_classes   : tclass NameMap.t
  ; context_globals   : ttype NameMap.t
  }

let insert_function_declaration ctx name targs tfunc block =
  let tmap = NameMap.find name ctx in NameMap.add name (TypeListMap.add targs (tfunc, block) tmap) ctx
let insert_type_function_declaration ctx name targs tfunc virtOver =
  let tmap = NameMap.find name ctx in NameMap.add name (TypeListMap.add targs (tfunc, virtOver, None) tmap) ctx

type typecheck_error  = TypecheckSyntaxType.location * typecheck_error'
and  typecheck_error' =
  | EUnknownVariable of name
  | EBadThis
  | EAlreadyDeclared of name
  | EMultipleFunctionDefinition of name * rtype list
  | EPointerToReference
  | EReferenceToPointer
  | EReferenceToReference
  | EUnknownType of name
  | EScopedVariable
  | EReferenceRHS
  | EExceptedLHS
  | ECantCompare
  | ECantReturn
  | EDereferenceNonPointer
  | EBadFieldAccess
  | EUnknownField of name * name
  | EMultipleField of name * name
  | EBadCast of ttype * ttype
  | ERBadCast of rtype * rtype
  | EExceptedNum
  | EBadReference

module TM = struct
  module EitherMonad = EitherT(struct type t = typecheck_error end)(IdentityMonad)
  module StateMonad  = StateT (struct type t = state_context end)(EitherMonad.Monad)
  module ReaderMonad = ReaderT(struct type t = reader_context end)(StateMonad.Monad)
  module M = ReaderMonad
  type 'a t                                             = 'a M.t
  let bind                                              = M.bind
  let return                                            = M.return
  let ask                                               = M.ask
  let local                                             = M.local
  let throwError e : 'a t                               = M.lift (StateMonad.lift (EitherMonad.throwError e))
  let get : state_context t                             = M.lift (StateMonad.get)
  let put s                                             = M.lift (StateMonad.put s)
  let modify f                                          = M.lift (StateMonad.modify f)
  let rec mapM : ('a -> 'b t) -> 'a list -> ('b list) t = fun f -> function
    | []    -> return []
    | x::xs -> bind (f x) (fun x' -> bind (mapM f xs) (fun xs' -> return (x'::xs')))
end


let const a b  = a
let (@>)       = TM.bind
let (@>>) m m' = m @> (fun _ -> m')

let is_pointer = function
  | Typepointer _ -> true
  | _ -> false

let rec qualify_type ty qu = match qu with
  | QVariable  -> TM.return (ty, false)
  | QPointer a -> qualify_type ty a
    @> (fun (ty', rf) ->
      if rf
        then TM.throwError (dummy_location, EPointerToReference)
      else if is_pointer ty'
        then TM.throwError (dummy_location, EReferenceToPointer)
        else TM.return (ty', true)
    )
  | QReference a -> qualify_type ty a 
    @> (fun (ty', rf) ->
      if rf
        then TM.throwError (dummy_location, EReferenceToReference)
        else TM.return (ty', true)
    )

let rec assert_defined_types = function
  | []    -> TM.return ()
  | t::ts -> assert_defined_type t @>> assert_defined_types ts
and assert_defined_type = function
  | Typenull                        -> TM.return ()
  | Typevoid                        -> TM.return ()
  | Typeint                         -> TM.return ()
  | Typeclass na                    ->
        TM.get
      @> (fun {context_classes = cls} ->
        if NameMap.mem na cls
          then TM.return ()
          else TM.throwError (dummy_location, EUnknownType na)
      )
  | Typepointer ty                  -> assert_defined_type ty
and assert_defined_types_r l = assert_defined_types (List.map (fun (ty, _) -> ty) l)
and assert_defined_type_r (ty, _) = assert_defined_type ty

let rec assert_valid_supers = function
  | []    -> TM.return ()
  | t::ts -> assert_valid_super t @>> assert_valid_supers ts
and assert_valid_super n = TM.get
  @> (fun {context_classes = ctx} ->
    if NameMap.mem n ctx
      then TM.return ()
      else TM.throwError (dummy_location, EUnknownType n)
  )

let rec is_subclass a b = if a = b
  then TM.return true
  else TM.get
    @> (fun {context_classes = ctx} ->
      let asup = (NameMap.find a ctx).class_supers in
      NameSet.fold (fun c m -> m @> (fun d -> is_subclass c b @> (fun e -> TM.return (d || e)))) asup (TM.return false)
    )

let rec get_class_members (a : name) : (NameSet.t * (class_unique_member * ttype) NameMap.t) TM.t = TM.get
  @> (fun {context_classes = ctx} -> get_class_members' @@ NameMap.find a ctx)
and get_class_members' (cls : tclass) : (NameSet.t * (class_unique_member * ttype) NameMap.t) TM.t =
  NameSet.fold
    (fun super m -> m
    @> (fun (mult, unique : NameSet.t * (class_unique_member * ttype) NameMap.t) ->
      get_class_members super
    @> (fun (supMult, supUnique : NameSet.t * (class_unique_member * ttype) NameMap.t) ->
      let omult = namemap_inter unique supUnique in
      let nmult = NameSet.union mult @@ NameSet.union supMult @@ omult in
      let nunique = NameMap.merge
        (fun _ v1 v2 -> match (v1, v2) with
          | (Some v, None)    -> Some v
          | (None, Some v)    -> Some v
          | (Some v, Some v') -> None
          | (None, None)      -> None
        ) unique supUnique in
      TM.return (nmult, nunique)
    )))
    cls.class_supers
    (TM.return (NameSet.empty, NameMap.mapi (fun k t -> (FLocal k, t)) cls.class_members))

let can_cast = function
  | Typevoid -> begin function
    | Typevoid -> TM.return true
    | _ -> TM.return false
    end
  | Typeint -> begin function
    | Typeint -> TM.return true
    | _ -> TM.return false
    end
  | Typenull -> begin function
    | Typenull | Typepointer _ -> TM.return true
    | _ -> TM.return false
    end
  | Typeclass a -> begin function
    | Typeclass b -> is_subclass a b
    | _ -> TM.return false
    end
  | Typepointer (Typeclass a) -> begin function
    | Typepointer (Typeclass b) -> is_subclass a b
    | _ -> TM.return false
    end
  | Typepointer a -> begin function
    | Typepointer a -> TM.return true
    | _ -> TM.return false
    end

let assert_can_cast loc a b =
  can_cast a b
  @> (fun c -> 
    if c
      then TM.return ()
      else TM.throwError (loc, EBadCast (a, b))
  )

let rec can_cast_list = function
  | [] -> begin function
    | []    -> TM.return true
    | y::ys -> TM.return false
    end
  | x::xs -> begin function
    | [] -> TM.return false
    | y::ys ->
        can_cast_list xs ys
      @> (fun b ->
        if b
          then can_cast x y
          else TM.return false
      )
  end

let can_cast_r (ty1, r1) (ty2, r2) =
  if r2 && (not r1)
    then TM.throwError (dummy_location, EBadReference)
    else can_cast ty1 ty2

let assert_can_cast_r loc a b =
  can_cast_r a b
  @> (fun c -> 
    if c
      then TM.return ()
      else TM.throwError (loc, ERBadCast (a, b))
  )

let rec can_cast_list_r = function
  | [] -> begin function
    | []    -> TM.return true
    | y::ys -> TM.return false
    end
  | x::xs -> begin function
    | [] -> TM.return false
    | y::ys ->
        can_cast_list_r xs ys
      @> (fun b ->
        if b
          then can_cast_r x y
          else TM.return false
      )
  end

let rec is_num = function
  | Typenull | Typeint | Typepointer _ -> true
  | Typeclass _ | Typevoid -> false
let is_num_r (ty, _) = is_num ty

let assert_is_num loc a = 
  if is_num a
    then TM.return ()
    else TM.throwError (loc, EExceptedNum)

let assert_lhs loc b =
  if b
    then TM.return ()
    else TM.throwError (loc, EExceptedLHS)

let rec make_class_declaration name def = function
  | []                     -> TM.return def
  | (m, loc)::ms           -> add_class_member name def loc m @> (fun def -> make_class_declaration name def ms)
and add_class_member name def loc = function
  | SourceSyntax.MVariable (ty, qu, name) -> begin match name with
    | SourceSyntaxType.Name n -> 
        qualify_type (type_of_name ty) qu
      @> (fun (typ, rf) ->
        if rf
          then TM.throwError (loc, EBadReference)
        else if NameMap.mem name def.class_members
          then TM.throwError (loc, EAlreadyDeclared name)
        else if NameMap.mem name def.class_functions
          then TM.throwError (loc, EAlreadyDeclared name)
          else assert_defined_type typ
            @>> TM.return {def with class_members = NameMap.add name typ def.class_members}
      )
    | SourceSyntaxType.ScopedName (ty, n) -> TM.throwError (loc, EScopedVariable)
    end
  | SourceSyntax.MFunction (((ty, qu, name), args), virt) -> begin match name with
    | SourceSyntaxType.Name n ->
        TM.mapM (fun (ty, qu, _) -> qualify_type (type_of_name ty) qu) args
      @> (fun targs ->
        qualify_type (type_of_name ty) qu
      @> (fun tfunc ->
        if NameMap.mem name def.class_members
          then TM.throwError (loc, EAlreadyDeclared name)
        else if NameMap.mem name def.class_functions
          then TM.throwError (loc, EAlreadyDeclared name)
          else assert_defined_types_r targs
            @>>
              assert_defined_type_r tfunc
            @>>
              (if virt
                then TM.return None
                else TM.get
                  @> (fun {context_classes = cls} -> TM.return @@ Some (
                    NameSet.filter
                    (fun sna -> NameMap.mem name (NameMap.find sna cls).class_functions)
                    def.class_supers
                  ))
              )
            @> (fun virtOver ->
              TM.return {def with class_functions = insert_type_function_declaration def.class_functions name targs tfunc virtOver}
            )
      ))
    | SourceSyntaxType.ScopedName (ty, n) -> TM.throwError (loc, EScopedVariable)
    end


let rec typecheck_program program : state_context TM.EitherMonad.t = 
  match typecheck_program' program
    { context_bindings = NameMap.empty
    ; context_current_class = None
    ; context_current_return_type = None
    }
    { context_functions = NameMap.empty
    ; context_classes = NameMap.empty
    ; context_globals = NameMap.empty
    }
  with
  | Left err      -> Left err
  | Right (s, ()) -> Right s
and typecheck_program' : SourceSyntax.declaration list -> unit TM.t 
                         = function
  | []    -> TM.return ()
  | d::ds -> typecheck_declaration d @> (fun d'  -> typecheck_program' ds)
and typecheck_declaration (d, loc) : unit TM.t
                                  = typecheck_declaration' loc d
and typecheck_declaration' loc : SourceSyntax.declaration' -> unit TM.t 
                              = function
  | SourceSyntax.DClass (name, supers, members) -> TM.get
    @> (fun {context_classes = ctx} -> match name with
      | SourceSyntaxType.Name n ->
        if NameMap.mem name ctx
          then TM.throwError (loc, EAlreadyDeclared name)
          else assert_valid_supers supers
            @>>
               make_class_declaration
                name
                { class_supers    = List.fold_right NameSet.add supers NameSet.empty
                ; class_functions = NameMap.empty
                ; class_members   = NameMap.empty
                ; class_unique_members = NameMap.empty
                } 
                members
            @> (fun def ->
              get_class_members' def
            @> (fun (_, cmem) ->
              let def' = {def with class_unique_members = cmem} in
              TM.modify (fun st -> {st with context_classes = NameMap.add name def ctx})
            ))
      | SourceSyntaxType.ScopedName (ty, n) -> TM.throwError (loc, EScopedVariable)
    )
  | SourceSyntax.DVariable (ty, qu, name) -> TM.get
    @> (fun {context_globals = cvar; context_functions = cfun} -> match name with
      | SourceSyntaxType.Name n ->
          qualify_type (type_of_name ty) qu
        @> (fun (typ, rf) ->
          if rf
            then TM.throwError (loc, EBadReference)
          else if NameMap.mem name cvar
            then TM.throwError (loc, EAlreadyDeclared name)
          else if NameMap.mem name cfun
            then TM.throwError (loc, EAlreadyDeclared name)
            else assert_defined_type typ
              @>>
                TM.modify (fun st -> {st with context_globals = NameMap.add name typ cvar})
        )
      | SourceSyntaxType.ScopedName (ty, n) -> TM.throwError (loc, EScopedVariable)
    )
  | SourceSyntax.DFunction (((ty, qu, name), args), block) -> TM.get
    @> (fun {context_globals = cvar; context_functions = cfun} -> match name with
      | SourceSyntaxType.Name n ->
          TM.mapM (fun (ty, qu, _) -> qualify_type (type_of_name ty) qu) args
        @> (fun targs ->
          qualify_type (type_of_name ty) qu
        @> (fun tfunc ->
          if NameMap.mem name cfun && TypeListMap.mem targs (NameMap.find name cfun)
            then TM.throwError (loc, EMultipleFunctionDefinition (name, targs))
          else if NameMap.mem name cvar
            then TM.throwError (loc, EAlreadyDeclared name)
            else assert_defined_types_r (tfunc :: targs)
              @>>
                typecheck_block block
              @> (fun block' ->
                TM.modify (fun st -> {st with context_functions = insert_function_declaration cfun name targs tfunc block'})
              )
        ))
      (* | SourceSyntaxType.ScopedName (ty, n) *)


    )
and typecheck_block : SourceSyntax.instruction list -> (TypecheckSyntax.instruction list) TM.t 
                   = function
  | []    -> TM.return []
  | i::is -> typecheck_instruction i
      @> (fun i'  -> TM.local (fun ctx -> {ctx with context_bindings = TypecheckSyntax.instruction_annotation i'}) (typecheck_block is)
      @> (fun is' -> 
        TM.return (i'::is')
      )
      )
and typecheck_instruction (i, (), loc) : TypecheckSyntax.instruction TM.t 
                                      = typecheck_instruction' loc i
  @> (fun (i', ctx) -> TM.return (i', ctx, loc))
and typecheck_instruction' loc : SourceSyntax.instruction' -> (TypecheckSyntax.instruction' * ttype NameMap.t) TM.t 
                              = function
  | SourceSyntax.IExpression e -> typecheck_expression e
      @> (fun e' -> TM.ask
      @> (fun {context_bindings = bindings} ->
        TM.return (TypecheckSyntax.IExpression e', bindings)
      ))
  | SourceSyntax.IVariable (ty, qu, na) ->
        qualify_type (type_of_name ty) qu
      @> (fun (qty, rf) ->
        if rf
          then TM.throwError (loc, EBadReference)
          else
            assert_defined_type qty
          @>>
            TM.ask
          @> (fun { context_bindings = bindings } ->
            TM.return (TypecheckSyntax.IVariable (ty, qu, na), NameMap.add na qty bindings)
          )
      )
  | SourceSyntax.IVariableAssign ((ty, qu, na), ex) ->
        qualify_type (type_of_name ty) qu
      @> (fun (qty, rf) ->
        assert_defined_type qty
      @>>
        typecheck_expression ex
      @> (fun ex' ->
        (if rf then assert_lhs (TypecheckSyntax.expression_location ex') (expression_lhs ex') else TM.return ())
      @>>
        assert_can_cast loc (expression_type ex') qty
      @>>
        TM.ask
      @> (fun { context_bindings = bindings } ->
        TM.return (TypecheckSyntax.IVariableAssign ((ty, qu, na), ex'), NameMap.add na qty bindings)
      )))
(*
  | SourceSyntax.IVariableConstruct  of variable_declaration * name * expression list
*)
  | SourceSyntax.IIf (c, ifB, elseB) ->
      typecheck_expression c
      @> (fun c' ->
        assert_can_cast loc (expression_type c') Typeint
      @>>
        typecheck_instruction ifB
      @> (fun ifB' ->
        typecheck_instruction elseB
      @> (fun elseB' ->
        TM.ask
      @> (fun {context_bindings = bindings} ->
        TM.return (TypecheckSyntax.IIf (c', ifB', elseB'), bindings)
      ))))
  | SourceSyntax.IWhile (c, i) ->
      typecheck_expression c
      @> (fun c' ->
        assert_can_cast loc (expression_type c') Typeint
      @>>
        typecheck_instruction i
      @> (fun i' ->
        TM.ask
      @> (fun {context_bindings = bindings} ->
        TM.return (TypecheckSyntax.IWhile (c', i'), bindings)
      )))
(*
  | SourceSyntax.IFor                of expression list * expression * expression list * instruction
*)
  | SourceSyntax.IBlock is ->
      typecheck_block is
      @> (fun is' ->
        TM.ask
      @> (fun {context_bindings = bindings} ->
        TM.return (TypecheckSyntax.IBlock is', bindings)
      ))
(*
  | SourceSyntax.ICout               of expression list
*)
  | SourceSyntax.IReturn e ->
        typecheck_expression e
      @> (fun e' ->
        TM.ask
      @> (fun {context_bindings = ctx; context_current_return_type = rtyopt} -> match rtyopt with
        | None -> TM.throwError (loc, ECantReturn)
        | Some rty ->
            assert_can_cast_r loc (expression_type_r e') rty
          @>>
            TM.return (TypecheckSyntax.IReturn e', ctx)
      ))
  | SourceSyntax.IReturnVoid   ->
        TM.ask
      @> (fun {context_bindings = ctx; context_current_return_type = rtyopt} -> match rtyopt with
        | None -> TM.return (TypecheckSyntax.IReturnVoid, ctx)
        | Some _ -> TM.throwError (loc, ECantReturn)
      )
and typecheck_expression (e, (), loc) : TypecheckSyntax.expression TM.t 
                                     = typecheck_expression' loc e
  @> (fun (e', ty, b) -> TM.return (e', (ty, b), loc))
and typecheck_expression' loc : SourceSyntax.expression' -> (TypecheckSyntax.expression' * ttype * bool) TM.t
                             = function
  | SourceSyntax.EInteger i     -> TM.return (TypecheckSyntax.EInteger i, Typeint, false)
  | SourceSyntax.ENull          -> TM.return (TypecheckSyntax.ENull, Typenull, false)
  | SourceSyntax.EThis                       -> TM.ask
      @> (fun {context_current_class = copt} ->
        match copt with
          | None     -> TM.throwError (loc, EBadThis) 
          | Some cls -> TM.return (TypecheckSyntax.EThis, Typepointer (Typeclass cls), false)
      )
  | SourceSyntax.EVariable name -> TM.ask
      @> (fun {context_bindings = ctx} ->
        if NameMap.mem name ctx
          then TM.return (TypecheckSyntax.EVariable name, NameMap.find name ctx, true)
          else TM.throwError (loc, EUnknownVariable name)
      )
  | SourceSyntax.EBinary (op, e1, e2) ->
      typecheck_expression e1
    @> (fun e1' ->
      typecheck_expression e2
    @> (fun e2' -> match op with
      | OpAssign ->
          assert_lhs loc (expression_lhs e1')
        @>>
          assert_can_cast loc (expression_type e2') (expression_type e1')
        @>>
            TM.return (TypecheckSyntax.EBinary (op, e1', e2'), expression_type e1', false)
      | OpEq | OpNEq ->
          assert_is_num loc (expression_type e1')
        @>>
          can_cast (expression_type e1') (expression_type e2')
        @> (fun c1 ->
          can_cast (expression_type e2') (expression_type e1')
        @> (fun c2 -> 
          if c1 || c2
            then
              TM.return (TypecheckSyntax.EBinary (op, e1', e2'), Typeint, false)
            else
              TM.throwError (loc, ECantCompare)
        ))
      | OpOr | OpAnd | OpLT | OpLTE | OpGT | OpGTE | OpAdd | OpSub | OpMul | OpDiv | OpMod -> 
          assert_can_cast loc (expression_type e1') Typeint
        @>>
          assert_can_cast loc (expression_type e2') Typeint
        @>>
          TM.return (TypecheckSyntax.EBinary (op, e1', e2'), Typeint, false)
    ))
  | SourceSyntax.EUnary (op, e)         ->
      typecheck_expression e
    @> (fun e' -> match op with
      | OpPreInc | OpPreDec | OpPostInc | OpPostDec ->
          assert_lhs loc (expression_lhs e')
        @>>
          assert_can_cast loc (expression_type e') Typeint
        @>>
          TM.return (TypecheckSyntax.EUnary (op, e'), Typeint, false)
      | OpNot | OpPlus | OpMinus ->
          assert_can_cast loc (expression_type e') Typeint
        @>>
          TM.return (TypecheckSyntax.EUnary (op, e'), Typeint, false)
      | OpReference ->
          assert_lhs loc (expression_lhs e')
        @>>
          TM.return (TypecheckSyntax.EUnary (op, e'), Typepointer (expression_type e'), false)
      | OpDereference -> begin match (expression_type e') with
        | Typepointer ty -> TM.return (TypecheckSyntax.EUnary (op, e'), ty, true)
        | _ -> TM.throwError (loc, EDereferenceNonPointer)
        end
    )
   | SourceSyntax.EField (e, f) ->
      typecheck_expression e
    @> (fun e' -> match expression_type e' with
      | Typeclass a -> TM.get
        @> (fun {context_classes = ctx} ->
          let acls = NameMap.find a ctx in
          if NameMap.mem f acls.class_unique_members
            then
              let (_, ty) = NameMap.find f acls.class_unique_members in
              TM.return (TypecheckSyntax.EField (e', f), ty, true)
            else TM.throwError (loc, EBadFieldAccess)
        )
      | _ -> TM.throwError (loc, EBadFieldAccess)
    )
(*   | SourceSyntax.ECall (f, args) ->
      TM.mapM typecheck_expression args
    @> (fun args' ->
      let targs = List.map expression_type args' in
    ) *)
(*
  | SourceSyntax.ENew       of name * expression list
*)
