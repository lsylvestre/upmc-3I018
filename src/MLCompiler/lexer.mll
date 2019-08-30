{
  open Parseutils
  open Parser        (* The type token is defined in parser.mli *)
}

let identifier = ['a'-'z''A'-'Z'] ['a'-'z''A'-'Z''0'-'9''_']*

rule token = parse
| ['0'-'9']+ as lxm  { INT(int_of_string lxm) }
| ';'                { SEMICOL }
| ','                { COMMA }
| '.'                { DOT }
| '?'                { QUESTION_MARK }
| ':'                { COLON }
| '='                { EQ }
| '{'                { LCURLY }
| '}'                { RCURLY }
| '['                { LBRACKET }
| ']'                { RBRACKET }
| '('                { LPAREN }
| ')'                { RPAREN }
| "var"              { VAR }
| "let"              { LET }
| "true"             { BOOL(true) }
| "false"            { BOOL(false) }
| "if"               { IF }
| "else"             { ELSE }
| "function"         { FUNCTION }
| "lambda"           { LAMBDA}
| "return"           { RETURN }
| "while"            { WHILE }
| "do"               { DO }

| ['+' '-']              as op      { PLUS_MINUS(String.make 1 op) }
| ['*' '/']              as op      { TIMES_DIV(String.make 1 op) }
| "**"                   as op      { POW(op) }
| "&&"                   as op      { AND(op) }
| "||"                   as op      { OR(op) }
| (['<' '>']'='?)        as op      { COMPARE(op) }
| (['!' '=']'=''='?)     as op      { EQUALITY(op) }
| (['+' '-' '*' '/' '&'] as op) '=' { OPEQ(String.make 1 op) }
| '%'                    as op      { MOD(String.make 1 op) }

| identifier as lxm  { IDENTIFIER(lxm) }
| ['\n' ]            { (Lexing.new_line lexbuf) ; (token lexbuf) }
| [' ' '\t']         { token lexbuf }    (* skip blanks *)
| "//"[^'\n']*       { token lexbuf }    (* skip c++ style line comments *)
| "/*"               { comment lexbuf }  (* skip c-style line comments *)
| eof                { EOF }
| _  as lxm          { raise (Parse_Exception (Printf.sprintf "Unexpected character: %c"  lxm,  default_position)) }



and comment = parse 
| "*/" { token lexbuf } 
| _    { comment lexbuf } 