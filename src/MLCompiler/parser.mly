%{
  let current_pos () = 
    Parseutils.make_position (Parsing.symbol_start_pos ())     
                             (Parsing.symbol_end_pos ())
%}

/* (* reserved words *) */
%token RETURN IF VAR ELSE IN WHILE DO FUNCTION LAMBDA LET

%token <string> IDENTIFIER
%token <int> INT
%token <bool> BOOL

%token <string> OPEQ AND OR EQUALITY COMPARE PLUS_MINUS MOD TIMES_DIV POW 
%token EQ 
%token QUESTION_MARK COLON


/* (* control characters *) */
%token EOF DOT COMMA SEMICOL LCURLY RCURLY LBRACKET RBRACKET LPAREN RPAREN

/* (* operators *) */

%right     QUESTION_MARK COLON   /* lowest precedence */ 
%left      OR
%left      AND
%left      EQUALITY
%left      COMPARE
%left      PLUS_MINUS   
%left      MOD       
%left      TIMES_DIV 
%right     POW             
%left      DOT                  
%nonassoc  LPAREN RPAREN         /* highest precedence */        

%start program         /* the entry point */

%type <Ast.program>    program
%type <Ast.statements> statements
%type <Ast.statement>  statement
%type <Ast.expr>       expr
%type <Ast.statement>  fundef
%type <Ast.statements> block
%type <string list>    parameters
%type <Ast.expr list>  arguments

%%

program:
 | /* empty */   { { filename = ""; body = [] } }
 | statements    { { filename = ""; body = $1 } }
 ;

statements: /***** pas de vide *****/
 | statement            { [$1] }
 | statement statements { $1 :: $2 }
 | block                {$1}
 | block statements { $1 @ $2 }
 | LET IDENTIFIER EQ expr SEMICOL { [ Let($2, $4,[], current_pos ()) ] }
 | LET IDENTIFIER EQ expr SEMICOL statements { [ Let($2,$4,$6, current_pos ()) ] }
 ;

statement:
 | SEMICOL                    { Empty (current_pos ())}
 | opened_statement SEMICOL   { $1 }
 | closed_statement           { $1 }
 ;

opened_statement:
 | IDENTIFIER EQ expr                    { Assign ($1, $3, current_pos ()) }
 | IDENTIFIER OPEQ expr                  { OpAssign ($2, $1, $3, current_pos ()) }
 | VAR IDENTIFIER EQ expr                { Var($2, $4, current_pos ()) }
 | expr                                  { VoidExpr ($1, current_pos ()) }
 | RETURN expr                           { Return($2, current_pos ()) }
 | DO block WHILE LPAREN expr RPAREN     { DoWhile($5, $2, current_pos ()) }
 ;

closed_statement:   
 | IF LPAREN expr RPAREN block            { If($3, $5,[], current_pos ()) } 
 | IF LPAREN expr RPAREN block ELSE block { If($3, $5, $7, current_pos ()) }
 | WHILE LPAREN expr RPAREN block         { While($3, $5, current_pos ()) }
 | fundef                                 { $1 }
 ;

fundef: 
 | FUNCTION IDENTIFIER LPAREN RPAREN block
    { Fundef($2,[], $5, current_pos ()) }
 | FUNCTION IDENTIFIER LPAREN parameters RPAREN block
    { Fundef($2,$4, $6, current_pos ()) }
 ;

block:
 | LCURLY RCURLY            { [] }
 | LCURLY statements RCURLY { $2 }
 ; 

parameters: 
  /* pas de vide () ou de (,,,) */
 | IDENTIFIER { [$1] }
 | IDENTIFIER COMMA parameters { $1::$3 }
 ;

expr:
 | INT                                   { IntConst($1, current_pos ()) }
 | BOOL                                  { BoolConst($1, current_pos ()) }
 | expr LPAREN arguments RPAREN          { Funcall($1,$3, current_pos ()) }
 | expr LPAREN RPAREN                    { Funcall($1,[], current_pos ()) }
 | LAMBDA LPAREN parameters RPAREN block { Lambda($3,$5,current_pos ()) }
 | LAMBDA LPAREN RPAREN block            { Lambda([],$4,current_pos ()) }
 | IDENTIFIER                            { EVar ($1, current_pos ()) }
 | expr POW expr                         { BinOp($2, $1, $3, current_pos ()) }
 | expr TIMES_DIV expr                   { BinOp($2, $1, $3, current_pos ()) }
 | expr PLUS_MINUS expr                  { BinOp($2, $1, $3, current_pos ()) }
 | expr COMPARE expr                     { BinOp($2, $1, $3, current_pos ()) }
 | expr EQUALITY expr                    { BinOp($2, $1, $3, current_pos ()) }
 | expr OR expr                          { BinOp($2, $1, $3, current_pos ()) }
 | expr AND expr                         { BinOp($2, $1, $3, current_pos ()) }
 | expr QUESTION_MARK expr COLON expr    { TernaryOperator($1,$3,$5, current_pos ()) }
 | LBRACKET expr COMMA expr RBRACKET     { Pair($2,$4,current_pos ()) }
 | expr DOT IDENTIFIER                   { Access($1,$3, current_pos ()) }
 | LPAREN expr RPAREN                    { $2 }
 ;       

arguments:
 /* pas de vide () ou de (,,,) */
 | expr { [$1] }
 | expr COMMA arguments { $1::$3 }
 ;
