
open Utils

type prim =
  { name :    string;
    id :      int;
    vm_name : string;
    doc :     string;
  }

(** Environnement de compilation pour les primitives. *)
type prim_env = prim StringMap.t

(** Référence de la primitive de nom [name] 
 dans l'environnement [prims] des primitives. *)
let prim_fetch (prims:prim_env) (name:string) : prim option =
  try Some (StringMap.find name prims)
  with Not_found -> None

let init_prim_lst =
  [{ name = "+"    ; id = 0  ; vm_name = "ADD"     ; doc = "Addition" };
   { name = "-"    ; id = 1  ; vm_name = "SUB"     ; doc = "Subtraction" };
   { name = "*"    ; id = 2  ; vm_name = "MUL"     ; doc = "Multiplication" };
   { name = "/"    ; id = 3  ; vm_name = "DIV"     ; doc = "Division" };
   { name = "**"   ; id = 4  ; vm_name = "POW"     ; doc = "Power of" };
   { name = "=="   ; id = 5  ; vm_name = "EQ"      ; doc = "equality" };
   { name = "<"    ; id = 6  ; vm_name = "INF"     ; doc = "Lower" };
   { name = ">"    ; id = 7  ; vm_name = "SUP"     ; doc = "Greater" };
   { name = "<="   ; id = 8  ; vm_name = "INFEQ"   ; doc = "Lower or equal" };
   { name = ">="   ; id = 9  ; vm_name = "SUPEQ"   ; doc = "Greater or equal" };
   { name = "cons" ; id = 10 ; vm_name = "CONS"    ; doc = "cons" };
   { name = "car"  ; id = 11 ; vm_name = "CAR"     ; doc = "car" };
   { name = "cdr"  ; id = 12 ; vm_name = "CDR"     ; doc = "cdr" };
   { name = "!="   ; id = 13 ; vm_name = "DIF"     ; doc = "inequal" };
   { name = "==="  ; id = 14 ; vm_name = "EQEQ"    ; doc = "strict equal" };
   { name = "!=="  ; id = 15 ; vm_name = "DIFEQ"   ; doc = "strict inequal" };
   { name = "&&"   ; id = 16 ; vm_name = "AND"   ; doc = "logical and" };
   { name = "||"   ; id = 17 ; vm_name = "OR"   ; doc = "logical or" };
   { name = "%"    ; id = 18 ; vm_name = "MODULO"   ; doc = "remainder" }
  ]

let build_prim_map (l:prim list) : prim_env =
  List.fold_left
    (fun pmap prim ->
      StringMap.add prim.name prim pmap) (StringMap.empty) l

let init_prim_env () =
  build_prim_map init_prim_lst
