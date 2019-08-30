
open Ast
open Kast

(** Erreurs de compilation. *)
let expand_error (msg:string) (pos:parse_pos) : 'a =
  Printf.(printf "Expansion error at line %d column %d\n" pos.start_pos.pos_lnum pos.start_pos.pos_cnum ;
          printf "  ==> %s\n" msg ;
          failwith "Abort compilation.")


let rec expand_statement (stmt:statement) : kstatement =
  match stmt with
  | Empty (pos)             -> KEmpty (pos) 
  | VoidExpr (expr, pos)    -> KVoidExpr (expand_expr expr, pos)
  | Var (name, expr, pos)   -> KVar (name, expand_expr expr, pos)
  | Let (id, e, body, pos) 
    -> let kf = KClosure([id], expand_statements body pos, pos) in
       KVoidExpr (KCall(kf,[expand_expr e], pos), pos)
  | Fundef (name, params, body, pos)
    -> KVar (name,(KClosure (params,expand_statements body pos,pos)), pos)
  | Assign (var, expr, pos) -> KAssign (var, expand_expr expr, pos)
  | OpAssign (op,var, expr, pos) -> Assign (var, BinOp(op,EVar(var,pos),expr,pos), pos) |> expand_statement
  | If (cond, thens, elses, pos) -> KIf (expand_expr cond,
                                    	 expand_statements thens pos,
                                    	 expand_statements elses pos,
                                    	 pos)
  | While (cond, body, pos) -> KWhile (expand_expr cond,
                                       expand_statements body pos,
                                       pos)
  | DoWhile (cond, body, pos) -> KDoWhile (expand_expr cond,
                                           expand_statements body pos,
                                           pos)
  | Return (expr, pos) -> KReturn (expand_expr expr, pos)
and expand_statements (stmts:statements) (pos:parse_pos) : kstatement =
  KSeq (List.map expand_statement stmts, pos)

and expand_expr (expr:expr) : kexpr =
  match expr with
  | IntConst (n, pos)      -> KInt (n, pos)
  | BoolConst (true, pos)  -> KTrue pos
  | BoolConst (false, pos) -> KFalse pos
  | EVar (var, pos)        -> KEVar (var, pos)
  | BinOp (op, lexpr, rexpr, pos) 
    -> expand_call op [lexpr;rexpr] pos
  | TernaryOperator (cond,then_expr,else_expr, pos)
    -> Funcall(Lambda ([],[ If(cond,[Return(then_expr,pos)],
                          [Return(else_expr,pos)], pos)], pos),[],pos) |> expand_expr
  | Funcall(fexpr, args, pos)
    -> KCall(expand_expr fexpr, List.map expand_expr args, pos)
  | Lambda (params, body, pos)
    -> KClosure(params, KSeq (List.map expand_statement body, pos), pos)
  | Pair (lexpr,rexpr,pos)
    -> expand_call "cons" [lexpr;rexpr] pos
  | Access (expr,name,pos)
    -> expand_call (String.lowercase_ascii name) [expr] pos

and expand_call (name:string) expressions pos =
  KCall (KEVar (name, pos), List.map expand_expr expressions, pos)

let expand_prog { filename=fname; body=stmts } : kprogram =
  { kfilename = fname; kbody = (List.map expand_statement stmts) }




