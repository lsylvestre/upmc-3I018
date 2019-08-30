
open Printf

open Parseutils

open Utils

type kprogram = { kfilename: string; kbody: kstatements }

and kstatements = kstatement list

and kstatement =
  | KEmpty of (parse_pos)
  (* <expr>  (drop result) *)
  | KVoidExpr of (kexpr * parse_pos)
  (* var name = <expr>    (global variable) *)
  | KVar of (string * kexpr * parse_pos)
  (* sequence *)
  | KSeq of (kstatements * parse_pos)
  (* <var> = <expr> *)
  | KAssign of (string * kexpr * parse_pos)
  (* while(<cond>){ <statements> ... } *)
  | KWhile of (kexpr * kstatement * parse_pos)
  | KDoWhile of (kexpr * kstatement * parse_pos)
  (* if(<cond>) { <consequent> ... } else { <alternative> ... } *)
  | KIf of (kexpr * kstatement * kstatement * parse_pos)
  (* return <expr>    (function return) *)
  | KReturn of (kexpr * parse_pos)

and kexpr =
  (* 1, 23, -12, etc. *)
  | KInt of (int * parse_pos)
  (* true, false *)
  | KTrue of parse_pos
  | KFalse of parse_pos
  (* variable *)
  | KEVar of (string * parse_pos)
  (* call *)
  | KCall of (kexpr * kexpr list * parse_pos)
  (*  function (param1, param2, ...) { <statements> ... } *)
  | KClosure of (string list * kstatement * parse_pos)

let position_of_kstatement (stmt:kstatement) : parse_pos =
  match stmt with
  | KEmpty (p) -> p
  | KVoidExpr (_, p) -> p
  | KVar (_, _, p) -> p
  | KSeq (_, p) -> p
  | KAssign (_, _, p) -> p
  | KWhile(_, _, p) -> p
  | KDoWhile(_, _, p) -> p
  | KIf(_, _, _, p) -> p
  | KReturn(_, p) -> p

let position_of_kexpr (expr:kexpr) : parse_pos =
  match expr with
  | KInt(_, p) -> p
  | KTrue p -> p
  | KFalse p -> p
  | KEVar(_, p) -> p
  | KCall(_, _, p) -> p
  | KClosure(_, _, p) -> p

let rec string_of_kprogram { kbody = instrs ; _ } : string =
  string_of_kstatements 0 instrs

and string_of_kstatements (indent:int) (instrs:kstatements) : string =
  Utils.string_join "\n" (List.map (string_of_kstatement indent) instrs)

and string_of_kstatement (indent:int) (instr:kstatement) : string =
  match instr with
  | KEmpty (_) -> ""
  | KVoidExpr (expr, _) ->
     sprintf "%sKVoidExpr[\n%s\n%s]"
       (indent_string indent)
       (string_of_kexpr (indent + 1) expr)
       (indent_string indent)
  | KVar (id, expr, _) ->
     sprintf "%sKVar(%s)[\n%s\n%s]"
       (indent_string indent)
       id
       (string_of_kexpr (indent + 1) expr)
       (indent_string indent)
  | KSeq (stmts, _) ->
     sprintf "%sKSeq[\n%s\n%s]"
       (indent_string indent)
       (string_of_kstatements (indent + 1) stmts)
       (indent_string indent)
  | KAssign(id, expr, _) -> sprintf "%sKAssign[\nKVar(%s) %s ]"
                            (indent_string indent) id (string_of_kexpr (indent + 1) expr)
  | KWhile(cond, body, _) ->
     sprintf "%sKWhile[\n%s\n%s<body>[\n%s\n%s]\n%s]"
       (indent_string indent)
       (string_of_kexpr (indent + 1) cond)
       (indent_string (indent + 1))
       (string_of_kstatement (indent + 2) body)
       (indent_string (indent + 1))
       (indent_string indent)
  | KDoWhile(cond, body, _) ->
     sprintf "%sKDoWhile[\n%s<body>[\n%s\n%s]\n%s\n%s]"
       (indent_string indent)
       (string_of_kstatement (indent + 2) body)
       (indent_string (indent + 1))
       (string_of_kexpr (indent + 1) cond)
       (indent_string (indent + 1))
       (indent_string indent)
  | KIf(cond, conseq, alter, _) ->
     sprintf "%sKIf[\n%s\n%s<then>[\n%s\n%s] <else>[\n%s\n%s]\n%s]"
       (indent_string indent)
       (string_of_kexpr (indent + 1) cond)
       (indent_string (indent + 1))
       (string_of_kstatement (indent + 2) conseq)
       (indent_string (indent + 1))
       (string_of_kstatement (indent + 2) alter)
       (indent_string (indent + 1))
       (indent_string indent)
  | KReturn(expr, _) ->
     sprintf "%sKReturn[\n%s]"
       (indent_string indent)
       (string_of_kexpr indent expr)
    
and string_of_kexpr (indent:int) (expr:kexpr) : string =
  match expr with
  | KInt (n, _) -> sprintf "%sKInt(%d)" (indent_string indent) n
  | KTrue _ -> sprintf "%sKTrue" (indent_string indent)
  | KFalse _ -> sprintf "%sKFalse" (indent_string indent)
  | KEVar (id, _) -> sprintf "%sKEVar(%s)" (indent_string indent) id
  | KCall (k,kl,_) -> sprintf "%sKCall[\n%s%s]" 
                        (indent_string indent) 
                        (string_of_kexpr (indent+1) k)
                        (List.fold_left (fun s e -> s ^ "\n" ^ string_of_kexpr (indent+1) e) "" kl)
  | KClosure (args,stmts,_) -> sprintf "%sKClosure(%s -> [\n%s])" 
                                 (indent_string indent) 
                                 (String.concat ", " args)   
                                 (string_of_kstatement indent stmts)
