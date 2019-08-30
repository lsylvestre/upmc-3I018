
open Constants

type bcinstr =
  (* global variables *)
  | BC_GALLOC
  | BC_GFETCH of int
  | BC_GSTORE of int
  (* stack *)
  | BC_PUSH of bcvalue
  | BC_POP
  (* lexical environment *)
  | BC_FETCH of int
  | BC_STORE of int
  (* control *)
  | BC_CALL of int
  | BC_RETURN
  (* jumps *)
  | BC_LABEL of int
  | BC_JUMP of int
  | BC_JFALSE of int
  | BC_JTRUE of int

and bcvalue =
  | BC_UNIT
  | BC_TRUE
  | BC_FALSE
  | BC_INT of int
  | BC_PRIM of int
  | BC_FUN of int

(* convertit l'instructions structurée instr en une liste de constantes *)
let rec list_of_bcinstr (instr:bcinstr) : bcconstants =
  match instr with
  | BC_GALLOC ->   [I_GALLOC]
  | BC_GFETCH n -> [I_GFETCH; Int n]
  | BC_GSTORE n -> [I_GSTORE; Int n]
  | BC_PUSH v ->    I_PUSH :: (list_of_bcvalue v)
  | BC_POP ->      [I_POP]
  | BC_FETCH n ->  [I_FETCH; Int n]
  | BC_STORE n ->  [I_STORE; Int n]
  | BC_CALL n ->   [I_CALL; Int n]
  | BC_RETURN ->   [I_RETURN]
  | BC_LABEL s ->  [Lbl s]
  | BC_JUMP s ->   [I_JUMP; GotoLbl s]
  | BC_JFALSE s -> [I_JFALSE; GotoLbl s]
  | BC_JTRUE s ->  [I_JTRUE; GotoLbl s]

(* convertit la valeur structurée v en une liste de constantes *)
and list_of_bcvalue (v:bcvalue) : bcconstants =
  match v with
  | BC_UNIT -> [T_UNIT]
  | BC_FALSE -> [T_BOOL; Int 0]
  | BC_TRUE -> [T_BOOL; Int 1]
  | BC_INT n -> [T_INT; Int n]
  | BC_PRIM n -> [T_PRIM; Int n]
  | BC_FUN lbl -> [T_FUN; GotoLbl lbl]

(* convertit la liste d'instructions structurées bc en une liste de constantes *)
and list_of_bytecode (bc:bcinstr list) : bcconstants =
  List.map list_of_bcinstr bc |> List.concat

(***************************************
 conversion en string "human-readable"
 ***************************************)

(* convertit l'instructions structurée instr en string *)
let rec string_of_instr (instr:bcinstr) : string = 
  match list_of_bcinstr instr with 
  | [Lbl n] -> Printf.sprintf "L%d:" n
  | constants -> "  " ^ Utils.string_join " " 
                               (List.map string_of_bcconstant constants)

(* convertit la liste d'instructions structurées bc en string *)
and string_of_bytecode (bc:bcinstr list) : string =
  Utils.string_join "\n" (List.map string_of_instr bc)

(***********************************************
 conversion en string interprétable par la VM
 ***********************************************)

(* convertit la liste d'instructions structurées bc en une string 
suivant le format numérique accepté par la VM. *)
let serialize_bytecode (bc:bcinstr list) : string = 
  list_of_bytecode bc |> serialize_constants
