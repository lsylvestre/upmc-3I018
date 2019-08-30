
open Printf

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
  | BC_LABEL of string
  | BC_JUMP of string
  | BC_JFALSE of string

 and bcvalue =
   | BC_UNIT
   | BC_TRUE
   | BC_FALSE
   | BC_INT of int
   | BC_PRIM of int
   | BC_FUN of string


let rec string_of_bcinstr (instr:bcinstr) : string =
  match instr with
  | BC_GALLOC -> "  GALLOC"
  | BC_GFETCH n -> sprintf "  GFETCH %d" n
  | BC_GSTORE n -> sprintf "  GSTORE %d" n
  | BC_PUSH v -> sprintf "  PUSH_%s" (string_of_bcvalue v)
  | BC_POP -> "  POP"
  | BC_FETCH n -> sprintf "  FETCH %d" n
  | BC_STORE n -> sprintf "  STORE %d" n
  | BC_CALL n -> sprintf "  CALL %d" n
  | BC_RETURN -> "  RETURN"
  | BC_LABEL s -> sprintf "%s:" s
  | BC_JUMP s -> sprintf "  JUMP %s" s
  | BC_JFALSE s -> sprintf "  JFALSE %s" s

and string_of_bcvalue (v:bcvalue) : string =
  match v with
  | BC_UNIT -> "UNIT"
  | BC_TRUE -> "BOOL true"
  | BC_FALSE -> "BOOL false"
  | BC_INT n -> (sprintf "INT %d" n)
  | BC_PRIM n -> (sprintf "PRIM %d" n)
  | BC_FUN lbl -> (sprintf "FUN %s" lbl)

let string_of_bytecode (bc:bcinstr list) : string =
  Utils.string_join "\n" (List.map string_of_bcinstr bc)

		    
