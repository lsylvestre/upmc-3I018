
type bcconstants = bcconstant list
and bcconstant = 
  (* entiers *)
  | Int of int 
  (* labels *)
  | Lbl of int
  (* références vers un label : en `argument` de JUMP, JFALSE et JTRUE *) 
  | GotoLbl of int

  (* instructions *)
  | I_LABEL 
  | I_GALLOC 
  | I_PUSH  
  | I_GSTORE
  | I_POP 
  | I_JUMP 
  | I_GFETCH 
  | I_CALL
  | I_RETURN
  | I_FETCH
  | I_JFALSE 
  | I_STORE
  | I_JTRUE

  (* types *)
  | T_UNIT 
  | T_INT 
  | T_PRIM 
  | T_FUN 
  | T_BOOL 
  | T_PAIR

let opcode_of_bcconstant = function
  | Int n -> n
  | Lbl lbl -> failwith "labels have not opcode"
  (* l'adresse `lbl` vers laquelle on se branche est calculée 
     dans une autre fonction qui a accès au bytecode entier *)
  | GotoLbl lbl -> failwith "unknowed context" 
  | I_LABEL   -> -1
  | I_GALLOC  -> 0
  | I_PUSH    -> 1
  | I_GSTORE  -> 2
  | I_POP     -> 3
  | I_JUMP    -> 4 
  | I_GFETCH  -> 5 
  | I_CALL    -> 6
  | I_RETURN  -> 7
  | I_FETCH   -> 8
  | I_JFALSE  -> 9 
  | I_STORE   -> 10
  | I_JTRUE   -> 11

  | T_UNIT -> 0 
  | T_INT -> 1 
  | T_PRIM  -> 2 
  | T_FUN -> 3 
  | T_BOOL -> 4 
  | T_PAIR -> 5

let string_of_bcconstant = function
  | Int n -> string_of_int n
  | Lbl n -> "L" ^ string_of_int n
  | GotoLbl n -> "L" ^ string_of_int n
  | I_LABEL   -> "LABEL"
  | I_GALLOC  -> "GALLOC"
  | I_PUSH    -> "PUSH"
  | I_GSTORE  -> "GSTORE"
  | I_POP     -> "POP"
  | I_JUMP    -> "JUMP"
  | I_GFETCH  -> "GFETCH"
  | I_CALL    -> "CALL"
  | I_RETURN  -> "RETURN"
  | I_FETCH   -> "FETCH"
  | I_JFALSE  -> "JFALSE"
  | I_STORE   -> "STORE"
  | I_JTRUE   -> "JTRUE"

  | T_UNIT -> "UNIT" 
  | T_INT -> "INT"
  | T_PRIM  -> "PRIM" 
  | T_FUN -> "FUN" 
  | T_BOOL -> "BOOL"
  | T_PAIR -> "PAIR"

let inames () = [I_LABEL;I_GALLOC;I_PUSH ;I_GSTORE;
                 I_POP;I_JUMP;I_GFETCH;I_CALL;
                 I_RETURN;I_FETCH;I_JFALSE;I_STORE;I_JTRUE]

and itypes () = [T_UNIT;T_INT;T_PRIM;T_FUN;T_BOOL;T_PAIR]

(* ================= serialize bcconstants ================= *) 

let rec serialize_constants (ctx : bcconstants) = 
  let rec aux = function 
    | [] -> []
    | Lbl lbl :: r -> aux r
    | GotoLbl lbl :: r -> (find_label_code lbl ctx) :: (aux r)
    | instr :: r -> (opcode_of_bcconstant instr) :: (aux r) 
  in
  let code_list = aux ctx in
  let len = List.length code_list in 
  Printf.sprintf "424242 %d %s \n" 
    len (Utils.string_join " " (List.map string_of_int code_list))

(** rend le nombre de bcconstant jusqu'au (Label n), 
  * autrement dit l'adresse du Label n dans le bytecode *)
and find_label_code n (l : bcconstants) =
  let rec aux i = function
  | [] -> failwith "find_label_code"
  | Lbl n' :: r -> if n' = n then i else aux i r
  | _ :: r -> aux (i+1) r in
  aux 0 l

(* ================= generate VM files constants.h /  constants.c ================= *)  

let generate_constants_h prim_env =
  let acc = ref "" in
  let accum s = acc := !acc ^ s in
  accum "/* Fichier généré automatiquement : ne pas éditer. */\n\n";
  accum "/* Constantes pour les opcodes */\n\n";
  List.iter (fun n -> let s = string_of_bcconstant n 
	              and op = opcode_of_bcconstant n in
	              accum (Printf.sprintf "/** opcode %s */\n#define I_%s %d\n\n" s s op)) (inames ());
  accum "/** Noms des opcodes */\nextern const char *opcode_names[];\n\n";
  accum "/* Constantes pour les types */\n\n";
  List.iter (fun n -> let s = string_of_bcconstant n 
	              and op = opcode_of_bcconstant n in
	              accum (Printf.sprintf "/** type %s */\n#define T_%s %d\n\n" s s op)) (itypes ());
  accum  "/** Noms des types */\nextern const char *type_names[];\n\n";
  accum "/* Constantes pour les primitives */\n\n";
  Utils.StringMap.iter (fun _ Prim.{ name =n; id = op; vm_name=v } ->
      accum (Printf.sprintf "/** primitive %s */\n#define P_%s %d\n\n" n v op)) prim_env;
  accum "/** Noms des primitives */\nextern const char *primitive_names[];\n";
  !acc

and generate_constants_c prim_env =
  let acc = ref "" in
  let accum s = acc := !acc ^ s in
  accum "/* Fichier généré automatiquement : ne pas éditer. */\n\n";
  accum "/** Noms des opcodes */\n";
  accum "const char *opcode_names[] = {\n";
  List.iter (fun n -> accum (Printf.sprintf "  \"%s\",\n" (string_of_bcconstant n))) (inames ());
  accum "  \"<unknown>\"\n};\n\n";
  accum "/** Noms des types */\n";
  accum "const char *type_names[] = {\n";
  List.iter (fun n -> accum (Printf.sprintf "  \"%s\",\n" (string_of_bcconstant n))) (itypes ());
  accum "  \"<unknown>\"\n};\n\n";

  accum "/** Noms des primitives */\n";
  accum "const char *primitive_names[] = {\n";
  Utils.StringMap.iter (fun _ Prim.{ name =n; id = op; vm_name=v } 
    -> accum (Printf.sprintf "  \"%s\",\n" n)) prim_env;
  accum "  \"<unknown>\"\n};\n\n";
  !acc
