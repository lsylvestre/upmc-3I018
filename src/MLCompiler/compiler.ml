
(**  Le module de compilation.

 *)

open Utils

open Parseutils
open Lexing

open Prim
open Kast
open Bytecode

open Printf

(** Erreurs de compilation. *)
let compile_error (msg:string) (pos:parse_pos) : 'a =
  printf "Compilation error at line %d column %d\n" pos.start_pos.pos_lnum pos.start_pos.pos_cnum ;
  printf "  ==> %s\n" msg ;
  failwith "Abort compilation."

(**  Fonction utilitaire pour l'accès aux
environnements globaux et locaux. *)
let fetch_index (xs:'a list) (x:'a) : int option =
  let rec fetch xs i = match xs with
    | [] -> None
    | y::xs' -> if y = x then Some i
                else fetch xs' (i+1)
  in
  fetch xs 0

(** Environnement de compilation pour
les variables globales. *)
type glob_env = (string list) ref

let string_of_genv (genv:glob_env) : string =
  let rec tostr genv index = match genv with
    | [] -> ""
    | v::genv' -> (sprintf "%s=%d" v index)
		 ^ (match genv' with
		    | [] -> ""
		    | _ -> ", " ^ (tostr genv' (index + 1)))
  in
  tostr !genv 0

(** Indice de la variable [var] dans l'environment
global [genv]. *)
let genv_fetch (genv:glob_env) (var:string) : int option =
  fetch_index !genv var

(** Extension de l'environnement global *)
let genv_extend (genv:glob_env) (var:string) pos : int =
  let rec extend genv gref ngenv =
    match genv with
    | [] -> (gref, (List.rev (var::ngenv)))
    | v::genv'
      -> if var = v then (compile_error (sprintf "Global variable '%s' already defined" var) pos)
	else extend genv' (gref + 1) (v::ngenv)
  in
  let (gref, genv') = extend !genv 0 [] in
  genv := genv' ;
  gref


(** Environnement de compilation
 pour les variables locales. *)
type lex_env = string list

(** Indice de la variable [var] dans l'environment
lexical [lenv]. *)
let lenv_fetch (lenv:lex_env) (var:string) : int option =
  fetch_index lenv var


let next_label, reset_label =
  let label_value = ref 1 in
  (fun () -> let s = sprintf "L%d" !label_value in
	  incr label_value ;
	  s),
  (fun () -> label_value := 1)

(** Compilation des expressions. *)
let rec compile_expr prims genv lenv (expr:kexpr) =
  match expr with
  | KEVar (var, pos) -> compile_var prims genv lenv var pos
  | KTrue _ -> [(BC_PUSH BC_TRUE)]
  | KFalse _ -> [(BC_PUSH BC_FALSE)]
  | KInt (n, _) -> [(BC_PUSH (BC_INT n))]
  | KCall(expr, args, pos)
    -> compile_call prims genv lenv expr args pos
  | KClosure (params, body, pos)
    -> compile_closure prims genv lenv params body pos
  | _ -> compile_error "Doesn't know (yet) how to compile expression." (position_of_kexpr expr)

(** Compilation des variables.
 *)
and compile_var prims genv lenv (var:string) pos =
  match lenv_fetch lenv var with
  | Some i -> [ (BC_FETCH i) ]
  | None ->
     match genv_fetch genv var with
     | Some i -> [ (BC_GFETCH i) ]
     | None ->
        match prim_fetch prims var with
        | Some { id = i } -> [ (BC_PUSH (BC_PRIM i)) ]
        | None -> compile_error (sprintf "Not in scope : %s" var) pos


and compile_call prims genv lenv (fexpr:kexpr) (args:kexpr list) pos =
  (mappend (fun arg -> compile_expr prims genv lenv arg) (List.rev args))
  @ (compile_expr prims genv lenv fexpr)
  @ [(BC_CALL (List.length args))]

and compile_closure prims genv lenv (params:string list) (body:kstatement) pos =
  let fun_lbl = next_label ()
  and cont_lbl = next_label ()
  and lenv' = params @ lenv
  in
  [ (BC_JUMP cont_lbl) ;
    (BC_LABEL fun_lbl) ]
  @ (compile_statement prims genv lenv' body)
  @ [ (BC_RETURN) ;
      (BC_LABEL cont_lbl) ;
      (BC_PUSH (BC_FUN fun_lbl)) ]

  and compile_statement prims genv lenv (stmt:kstatement) =
  match stmt with
  | KVoidExpr (expr, pos)
    -> compile_expr prims genv lenv expr
      @ [(BC_POP)] (* nettoyer la pile *)
  | KVar (var, expr, pos)
    -> compile_gvar prims genv lenv var expr pos
  | KAssign (var, expr, pos)
    -> compile_assign prims genv lenv var expr pos
  | KSeq (stmts, pos)
    -> compile_seq prims genv lenv stmts
  | KIf(cond, conseq, alter, pos)
    -> compile_if prims genv lenv cond conseq alter pos
  | KReturn (expr, pos)
    -> compile_return prims genv lenv expr pos
  | _ -> compile_error "Don't know (yet) how to compile statement." (position_of_kstatement stmt)

and compile_gvar prims genv lenv (gvar:string) (expr:kexpr) pos =
  let gref = genv_extend genv gvar pos in
  (BC_GALLOC)::
    (compile_expr prims genv lenv expr)
  @ [(BC_GSTORE gref)]

and compile_assign prims genv lenv (var:string) (expr:kexpr) pos =
  (compile_expr prims genv lenv expr)
  @ (match lenv_fetch lenv var with
     | Some i -> [ (BC_STORE i) ]
     | None -> (match genv_fetch genv var with
	       | Some i -> [ (BC_GSTORE i) ]
	       | None -> compile_error (sprintf "Not in scope: %s" var) pos))

and compile_if prims genv lenv (cond:kexpr) (then_stmt:kstatement) (else_stmt:kstatement) pos =
  let onfalse = next_label ()
  and contlbl = next_label () in
  (compile_expr prims genv lenv cond)
  @ [(BC_JFALSE onfalse)]
  @ (compile_statement prims genv lenv then_stmt)
  @ [(BC_JUMP contlbl) ;
     (BC_LABEL onfalse)]
  @ (compile_statement prims genv lenv else_stmt)
  @ [(BC_LABEL contlbl)]

and compile_return prims genv lenv (expr:kexpr) pos =
  (compile_expr prims genv lenv expr)
  @ [(BC_RETURN)]

and compile_seq (prims:prim_env) (genv:glob_env) (lenv:lex_env) (stmts:kstatement list) : bcinstr list =
  List.fold_left (fun res stmt -> res @ (compile_statement prims genv lenv stmt)) [] stmts

let compile_prog (prims:prim_env) { kfilename=_; kbody=body } : bcinstr list =
  reset_label () ;
  let genv = ref [] in
  compile_seq prims genv [] body
