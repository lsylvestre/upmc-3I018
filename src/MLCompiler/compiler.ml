open Printf
open Kast
open Bytecode

(** Erreurs de compilation. *)
let compile_error (msg:string) (pos:Parseutils.parse_pos) : 'a =
  printf "Compilation error at line %d column %d\n" pos.start_pos.pos_lnum pos.start_pos.pos_cnum ;
  printf "  ==> %s\n" msg ;
  failwith "Abort compilation."
  
(** Générateur de labels *)
module GenLabels = struct 
  let label_value = ref 1 
  let reset () = label_value := 1
  and next () = let s = !label_value in 
                incr label_value; s
end

(** Environnement de compilation *)
module Env : sig
  type t = { prims : Prim.prim_env;
             genv : glob_env;
             lenv : lex_env }
  and glob_env                 
  and lex_env
  val create : Prim.prim_env -> t
  val genv_fetch : glob_env -> string -> int option
  val genv_extend : glob_env -> string -> Parseutils.parse_pos -> int
  val lenv_fetch : lex_env -> string -> int option
  val lenv_extend : t -> string list -> t
end = struct 
  type t = { prims : Prim.prim_env;
             genv : glob_env;
             lenv : lex_env }
  and glob_env = env ref  (* Environnement global *)                  
  and lex_env = env       (* Environnement lexical *)
  and env = string list

  (** création de l'environnement de compilation à partir de l'environnement de primitives [prims] *)
  let create (prims : Prim.prim_env) : t = {prims = prims; lenv = []; genv = ref []}

  (**  fonctions utilitaires pour l'accès aux environnements globaux et locaux. *)
  let fetch_index (xs:env) (x:'a) : int option =
    let rec fetch xs i = match xs with
      | [] -> None
      | y::xs' -> if y = x then Some i else fetch xs' (i+1) in
    fetch xs 0

  (** Indice de la variable [var] dans l'environnement global [genv]. *)
  let genv_fetch (genv:glob_env) (var:string) : int option =
    fetch_index !genv var

  (** Extension de l'environnement global *)
  let genv_extend (genv:glob_env) (var:string) pos : int =
    let rec extend genv gref ngenv =
      match genv with
      | [] -> (gref, (List.rev (var::ngenv)))
      | v::genv' -> if var = v then (compile_error (sprintf "Global variable '%s' already defined" var) pos)
  	                else extend genv' (gref + 1) (v::ngenv) in
    let (gref, genv') = extend !genv 0 [] in
    genv := genv'; 
    gref

  (** Indice de la variable [var] dans l'environnement lexical [lenv]. *)
  let lenv_fetch (lenv:lex_env) (var:string) : int option =
    fetch_index lenv var

  (** extension de l'environnement lexical *)
  let lenv_extend (env : t) (vars : string list) : t = 
    {env with lenv = (vars @ env.lenv)}

end (* fin du module Env *)

(** Compilation des expressions. *)
let rec compile_expr (env : Env.t) (expr:kexpr) =
  match expr with
  | KEVar (var, pos)             -> compile_var env var pos
  | KTrue _                      -> [(BC_PUSH BC_TRUE)]
  | KFalse _                     -> [(BC_PUSH BC_FALSE)]
  | KInt (n, _)                  -> [(BC_PUSH (BC_INT n))]
  | KCall(expr, args, pos)       -> compile_call env expr args pos
  | KClosure (params, body, pos) -> compile_closure env params body pos

(** Compilation des occurrences de variables. *)
and compile_var (env : Env.t) (var:string) pos =
  match Env.lenv_fetch env.lenv var with
  | Some i -> [ (BC_FETCH i) ]
  | None ->
     (match Env.genv_fetch env.genv var with
      | Some i -> [ (BC_GFETCH i) ]
      | None ->
         (match Prim.prim_fetch env.prims var with
          | Some { id = i } -> [ (BC_PUSH (BC_PRIM i)) ]
          | None -> compile_error (sprintf "Not in scope : %s" var) pos))

(** Compilation des appels de fonctions (et opérateurs) *)
and compile_call (env : Env.t) (fexpr:kexpr) (args:kexpr list) pos =
  (Utils.mappend (fun arg -> compile_expr env arg) (List.rev args))
  @ (compile_expr env fexpr)
  @ [(BC_CALL (List.length args))]

(** compilation des fermetures *)
and compile_closure (env : Env.t) (params:string list) (body:kstatement) pos =
  let fun_lbl = GenLabels.next ()
  and cont_lbl = GenLabels.next ()
  and env' = Env.lenv_extend env params in
  [ (BC_JUMP cont_lbl) ;
    (BC_LABEL fun_lbl) ]
  @ (compile_statement env' body)
  (* par sécurité  (retour "forcé") *)
  @ [ (BC_PUSH BC_UNIT) ;
      (BC_RETURN) ;
      (BC_LABEL cont_lbl) ;
      (BC_PUSH (BC_FUN fun_lbl)) ]

(** compilation des statements *)
and compile_statement (env : Env.t) (stmt:kstatement) =
  match stmt with
  | KEmpty (_)                    -> []
  | KVoidExpr (expr, pos)         -> compile_expr env expr
                                     @ [(BC_POP)] (* nettoyer la pile *)
  | KVar (var, expr, pos)         -> compile_gvar env var expr pos
  | KAssign (var, expr, pos)      -> compile_assign env var expr pos
  | KSeq (stmts, pos)             -> compile_seq env stmts
  | KIf(cond, conseq, alter, pos) -> compile_if env cond conseq alter pos
  | KWhile(cond, body, pos)       -> compile_while env cond body pos
  | KDoWhile(cond, body, pos)     -> compile_dowhile env cond body pos
  | KReturn (expr, pos)           -> compile_return env expr pos
                                   
(** Compilation des déclarations de variables globales *)                                
and compile_gvar (env : Env.t) (gvar:string) (expr:kexpr) pos =
  let gref = Env.genv_extend env.genv gvar pos in
  (BC_GALLOC)::
    (compile_expr env expr)
  @ [(BC_GSTORE gref)]

(** Compilation des affectations (variables globales) *) 
and compile_assign (env : Env.t) (var:string) (expr:kexpr) pos =
  (compile_expr env expr)
  @ (match Env.lenv_fetch env.lenv var with
     | Some i -> [ (BC_STORE i) ]
     | None -> (match Env.genv_fetch env.genv var with
      	        | Some i -> [ (BC_GSTORE i) ]
      	        | None -> compile_error (sprintf "Not in scope: %s" var) pos))

(** Compilation du statement return *)
and compile_return (env : Env.t) (expr:kexpr) pos =
  (compile_expr env expr)
  @ [(BC_RETURN)]
(** Compilation de l'alternative *)
and compile_if (env : Env.t) (cond:kexpr) (then_stmt:kstatement) (else_stmt:kstatement) pos =
  let onfalse_label = GenLabels.next ()
  and continuation_label = GenLabels.next () in
  (compile_expr env cond)
  @ [(BC_JFALSE onfalse_label)]
  @ (compile_statement env then_stmt)
  @ [(BC_JUMP continuation_label);
     (BC_LABEL onfalse_label)]
  @ (compile_statement env else_stmt)
  @ [(BC_LABEL continuation_label)]  

(** Compilation des boucles *)
and compile_while (env : Env.t) (cond:kexpr) (body:kstatement) pos = 
  (* à la KNUTH, avec l'instruction JTRUE de la VM. *)
  let cond_label = GenLabels.next ()
  and body_label = GenLabels.next () in  
  [(BC_JUMP cond_label)]
  @ [(BC_LABEL body_label)]
  @ (compile_statement env body)
  @ [(BC_LABEL cond_label)]
  @ (compile_expr env cond)
  @ [(BC_JTRUE body_label)]

and compile_dowhile (env : Env.t) (cond:kexpr) (body:kstatement) pos = 
  let begin_label = GenLabels.next () in  
  [(BC_LABEL begin_label)]
  @ (compile_statement env body)
  @ (compile_expr env cond)
  @ [(BC_JTRUE begin_label)] 

(** Compilation des séquences de statements *)
and compile_seq (env : Env.t) (stmts:kstatement list) : bcinstr list =
  List.fold_left (fun res stmt -> res @ (compile_statement env stmt)) [] stmts

(** Compilation du programme *)
let compile_prog (prims:Prim.prim_env) { kfilename=_; kbody=body } : bcinstr list =
  GenLabels.reset ();
  let env = Env.create prims in
  compile_seq env body
