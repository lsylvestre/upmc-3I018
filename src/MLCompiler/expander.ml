
open Printf

open Parseutils
open Lexing

open Ast
open Kast

(** Erreurs de compilation. *)
let expand_error (msg:string) (pos:parse_pos) : 'a =
  printf "Expansion error at line %d column %d\n" pos.start_pos.pos_lnum pos.start_pos.pos_cnum ;
  printf "  ==> %s\n" msg ;
  failwith "Abort compilation."


let rec expand_statement (stmt:statement) : kstatement =
  match stmt with
  | VoidExpr (expr, pos)
    -> KVoidExpr (expand_expr expr, pos)
  | Var (name, expr, pos)
    -> KVar (name, expand_expr expr, pos)
  | Assign (var, expr, pos)
    -> KAssign (var, expand_expr expr, pos)
  | If (cond, thens, elses, pos)
    -> KIf (expand_expr cond,
	   KSeq (List.map expand_statement thens, pos),
	   KSeq (List.map expand_statement elses, pos),
	   pos)
  | Fundef (name, params, body, pos)
    -> KVar (name,
             (KClosure (params,
                                  KSeq (List.map expand_statement body, pos),
                                  pos)),
             pos)

  | Return (expr, pos)
    -> KReturn (expand_expr expr, pos)

  | _ -> expand_error "Don't know how to expand statement"
		     (position_of_statement stmt)

and expand_expr (expr:expr) : kexpr =
  match expr with
  | IntConst (n, pos)
    -> KInt (n, pos)
  | BoolConst (true, pos)
    -> KTrue pos
  | BoolConst (false, pos)
    -> KFalse pos
  | EVar (var, pos)
    -> KEVar (var, pos)
  | BinOp (op, lexpr, rexpr, pos) ->
     expand_binop op (expand_expr lexpr) (expand_expr rexpr) pos
  | Funcall(fexpr, args, pos)
    -> KCall(expand_expr fexpr, List.map expand_expr args, pos)
  | Closuredef (params, body, pos)
    -> KClosure(params, KSeq (List.map expand_statement body, pos), pos)
  | _ -> expand_error "Don't know how to expand expression"
		     (position_of_expr expr)

and expand_binop (op:binop_name) (lexpr:kexpr) (rexpr:kexpr) pos =
  KCall (KEVar (expand_binop_prim op pos, pos), [lexpr; rexpr], pos)

and expand_binop_prim (op:binop_name) pos =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | BEq -> "=="
  | _ -> expand_error (sprintf "Don't know (yet) how to expand binary operator: %s"
                               (string_of_binop op)) pos

let expand_prog { filename=fname; body=stmts } : kprogram =
  { kfilename = fname; kbody = (List.map expand_statement stmts) }




