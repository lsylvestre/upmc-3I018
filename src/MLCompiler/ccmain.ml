open Printf
open Parseutils
open Ast
open Prim
open Kast
open Expander
open Compiler
open Bytecode

type control_mode =
  | GEN_CONSTANTS
  | PARSE_ONLY
  | PARSE_AND_EXPAND
  | COMPILE_AND_SHOW_BYTECODE
  | COMPILE_AND_GENERATE_TARGET
  | COMPILE_AND_RUN
  | COMPILE_AND_RUN_VMDEBUG


let abort (msg:string) (err_code:int) : unit =
  printf "Now quitting\n  ==> %s\n\nBye bye !\n" msg ;
  exit err_code

let parse_file (filename:string) : program =
  try
    (let in_file = open_in filename in
     let lexbuf = Lexing.from_channel in_file in
     let prog = Parser.program Lexer.token lexbuf in
     close_in in_file ;
     { prog with filename = filename } )
  with Parse_Exception (msg, pos) ->
    ( printf "Parse error: %s\n(%s)\n" msg (string_of_position pos) ) ;
    exit 1

let _ =
  printf "Microjs MLCompiler v0.0.2 (april 2019)\n" ;
  printf "-------------------------\n" ;

  let mode = ref COMPILE_AND_GENERATE_TARGET 
  and src_filename = ref "" in

  Arg.parse [("-parse", (Arg.Unit (fun () -> mode := PARSE_ONLY)),
    	                   "Parse and show parsed program");
    	      ("-expand", (Arg.Unit (fun () -> mode := PARSE_AND_EXPAND)),
    	                  "Parse, expand and show kernel abstract syntax tree");
    	      ("-compile", (Arg.Unit (fun () -> mode := COMPILE_AND_SHOW_BYTECODE)),
    	                  "Compile and show bytecode");
    	      ("-gen",  (Arg.Unit (fun () -> mode := COMPILE_AND_GENERATE_TARGET)),
    	                  "Compile and generate target (default mode)");
            ("-vmconst", (Arg.Unit (fun () -> mode := GEN_CONSTANTS)),
                        "Generate the constants for the VM");
    	      ("-run",  (Arg.Unit (fun () -> mode := COMPILE_AND_RUN)),
    	       "Compile and run program");
            ("-rundebug",  (Arg.Unit (fun () -> mode := COMPILE_AND_RUN_VMDEBUG)),
             "Compile and run program (debug mode)")
             ]
    (fun s -> src_filename := s) "Usage:\n  compiler [opts] <source_file>\nopts:" ;

  let prim_env = init_prim_env () in

  (if !mode = GEN_CONSTANTS then
     (Printf.printf "Generate header file: constants.h\n";
      Utils.print_oc "constants.h"  (Constants.generate_constants_h prim_env);
      Utils.print_oc "constants.c" (Constants.generate_constants_c prim_env);
      abort "Constants generation successful" 0));

  (if !src_filename = "" then
     abort "No source file..." 0) ;

  printf "[1] Parsing source file: %s ...\n" !src_filename ;
  let prog = 
    try parse_file !src_filename with 
    | Parse_Exception (s,pos) -> failwith (Printf.sprintf "Parse Error at %s : %s" (string_of_position pos) s)
    | Stdlib.Parsing.Parse_error -> failwith "unknown parse error"
  in

  printf "... parsing done.\n";

  (if !mode = PARSE_ONLY
   then ( printf "Parsed program:\n===========================\n%s\n===========================\n" 
            (string_of_program prog);
	        abort "I could compile, you know..." 0) );

  printf "[2] Expanding ...\n" ;

  let kprog = expand_prog prog
  in

  printf "... expansion done.\n";

  (if !mode = PARSE_AND_EXPAND
   then ( printf "Kernal Abstract Syntax Tree:\n===========================\n%s\n===========================\n" 
            (string_of_kprogram kprog);
	        abort "I could compile, you know..." 0) );

  printf "[3] Compiling ...\n" ;

  let target = compile_prog prim_env kprog in

  printf "... compilation done.\n";

  (if !mode = COMPILE_AND_SHOW_BYTECODE
   then ( printf "Bytecode:\n===========================\n%s\n===========================\n" 
              (string_of_bytecode target);
	        abort "I could generate the target, you know..." 0) );

  Printf.printf "[4] Serializing ..."; 
  let bcFilename = !src_filename ^ ".bc" in

  Utils.print_oc bcFilename (serialize_bytecode target);

  Printf.printf "... serialized to file '%s'\n" bcFilename;
  
(if !mode = COMPILE_AND_GENERATE_TARGET 
  then abort "I could run, you know..." 0);

let vm_path = "../NativeVM/"
and vm_name = "runvm"
and cc_path = "../MLCompiler/" in
let arguments = match !mode with
                | COMPILE_AND_RUN_VMDEBUG -> [|vm_name;"-d";cc_path ^ bcFilename|]
                | _ -> [|vm_name;cc_path ^ bcFilename|] in

printf "Nothing left to do, I can rest.";
Unix.execvp (vm_path ^ vm_name) arguments
   