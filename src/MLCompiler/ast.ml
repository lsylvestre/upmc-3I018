
open Printf

type parse_pos = Parseutils.parse_pos

type program = { filename: string; body: statements }

and statements = statement list

and statement =
  (* a null statement in the JCompiler *)
  | Empty of (parse_pos)                  
  (* function fname(param1, param2, ...) *)
  | Fundef of (string * string list * statements * parse_pos) 
  (* <expr>  (drop result) *)  
  | VoidExpr of (expr * parse_pos)                 
  (* var name = <expr>    (global variable) *)
  | Var of (string * expr * parse_pos)              
  (* let name = <expr>   (lexical variable) *)
  | Let of (string * expr * statements * parse_pos)   
  (* <var>[<pos>] = <expr> *)
  | Assign of (string * expr * parse_pos) 
  (* <var>[<pos>] [<op>]= <expr> *)    
  | OpAssign of (string * string * expr * parse_pos)        
  | While of (expr * statements * parse_pos)
  | DoWhile of (expr * statements * parse_pos)
  (* if(<cond>) { <consequent> ... } *)
  (* if(<cond>) { <consequent> ... } else { <alternative> ... } *)
  | If of (expr * statements * statements * parse_pos)
  (* return <expr>    (function return) *)
  | Return of (expr * parse_pos)

and expr =
  (* 1, 23, -12, etc. *)
  | IntConst of (int * parse_pos)
  (* true, false *)
  | BoolConst of (bool * parse_pos)
  | EVar of (string * parse_pos)
  (* <expr>  <BINOP> <expr> *)
  | BinOp of (binop * expr * expr * parse_pos)
  (* <UNOP> <expr> *)
  | Funcall of (expr * expr list * parse_pos)
  (*  function (param1, param2, ...) { <statements> ... } *)
  | Lambda of (string list * statements * parse_pos)
  | Pair of (expr * expr * parse_pos)
  (* e.car *)
  | Access of (expr * string * parse_pos)
  | TernaryOperator of (expr * expr * expr * parse_pos) (* ()?():() *)
and binop = string
 
(* ====== positions ===== *)

let position_of_statement (stmt:statement) : parse_pos =
  match stmt with
  | Empty (p) -> p
  | Fundef (_, _, _, p) -> p
  | VoidExpr (_, p) -> p
  | Var (_, _, p) -> p
  | Let (_, _, _, p) -> p
  | Assign (_, _, p) -> p
  | OpAssign (_, _, _, p) -> p
  | While(_, _, p) -> p
  | DoWhile(_, _, p) -> p
  | If(_, _, _, p) -> p
  | Return(_, p) -> p

and position_of_expr (expr:expr) : parse_pos =
  match expr with
  | IntConst(_, p) -> p
  | BoolConst(_, p) -> p
  | EVar(_, p) -> p
  | BinOp(_, _, _, p) -> p
  | TernaryOperator(_,_,_,p) -> p
  | Funcall(_, _, p) -> p
  | Lambda(_, _, p) -> p
  | Pair(_, _, p) -> p
  | Access(_, _, p) -> p

let indent_factor = 2

let indent_string (level:int) : string =
  String.make (level * indent_factor) ' '


(* ====== string of ast-node ===== *)

let rec string_of_program { body = instrs ; _ } : string =
  string_of_statements 0 instrs

and string_of_statements (indent:int) (instrs:statements) : string =
  Utils.string_join "\n" (List.map (string_of_statement indent) instrs) ^ "\n"

and string_of_statement (indent:int) (instr:statement) : string =
  match instr with
  | Empty (_) -> ""
  | Fundef (fvar, params, body, _) ->
     sprintf "%sfunction %s(%s) {\n%s%s}"
       (indent_string indent)
       fvar
       (Utils.string_join ", " params)
       (string_of_statements (indent + 1) body)
       (indent_string indent)
  | VoidExpr (expr, _) -> string_of_expr indent expr ^ ";"
  | Var (id, expr, _) -> sprintf "%svar %s = %s;" (indent_string indent) id (string_of_expr 0 expr)
  | Let (id, expr, body, _) -> sprintf "%slet %s = %s;\n%s"
                                 (indent_string indent) 
                                 id 
                                 (string_of_expr 0 expr)
                                 (string_of_statements indent body)
                             
  | Assign(id, expr, _) -> sprintf "%s%s = %s;" (indent_string indent) id (string_of_expr 0 expr)
  | OpAssign(op,id, expr, _) -> sprintf "%s%s %s= %s;" (indent_string indent) id op (string_of_expr 0 expr)
  | While(cond, body, _) ->
     sprintf "%swhile (%s) {\n%s\n%s}"
       (indent_string indent)
       (string_of_expr 0 cond)
       (string_of_statements (indent + 1) body)
       (indent_string indent)
  | DoWhile(cond, body, _) ->
     sprintf "%sdo {\n%s\n%s} while (%s);"
       (indent_string indent)
       (string_of_statements (indent + 1) body)
       (indent_string indent)
       (string_of_expr 0 cond)
  | If(cond, conseq, alter, _) ->
     sprintf "%sif (%s) {\n%s%s} else {\n%s%s}"
       (indent_string indent)
       (string_of_expr 0 cond)
       (string_of_statements (indent + 1) conseq)
       (indent_string indent)
       (string_of_statements (indent + 1) alter)
       (indent_string indent)                                                 
  | Return(expr, _) 
    -> sprintf "%sreturn %s;" (indent_string indent) (string_of_expr 0 expr)


and string_of_expr (indent:int) (expr:expr) : string =
  match expr with
  | IntConst (n, _) -> sprintf "%s%d" (indent_string indent) n
  | BoolConst (true, _) -> sprintf "%strue" (indent_string indent)
  | BoolConst (false, _) -> sprintf "%sfalse" (indent_string indent)
  | EVar (id, _) -> sprintf "%s%s" (indent_string indent) id
  | BinOp (op, e1, e2, _) -> sprintf "%s(%s %s %s)"
                               (indent_string indent) 
                               (string_of_expr 0 e1)
                               op  
                               (string_of_expr 0 e2)
  | TernaryOperator (cond, e1, e2, _) -> sprintf "%s(%s ? %s : %s)"
                                         (indent_string indent) 
                                         (string_of_expr 0 cond)
                                         (string_of_expr 0 e1)   
                                         (string_of_expr 0 e2)
  | Funcall(fexpr, arguments, _) ->
     sprintf "%s%s(%s)"
       (indent_string indent)
       (string_of_expr 0 fexpr)
       (Utils.string_join ", " (List.map (string_of_expr 0) arguments))
  | Lambda (args,body,_) -> sprintf "%slambda(%s){\n%s\n%s}"
                              (indent_string indent)
                              (Utils.string_join ", " args)
                              (string_of_statements (indent + 1) body)
                              (indent_string indent)
  | Pair (e1,e2,_) -> sprintf "%s[%s, %s]" 
                        (indent_string indent)
                        (string_of_expr 0 e1)
                        (string_of_expr 0 e2)
  | Access (e,name,_) ->  sprintf "%s%s.%s"
                            (indent_string indent)
                            (string_of_expr indent e)
                            name
