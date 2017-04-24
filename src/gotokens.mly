/* Separate token specification to make the definition visible
    to both lexer and parameterized parser */

%token FUNC GO VAR PKG MAIN PRINT MAKE CHAN INT COLONEQ ARROW
%token FOR ASSERT ENSURE /*WHILE*/ IF ELSE SELECT CASE
%token NOT AND OR EOF LPAR RPAR LBRACE RBRACE EQ NEQ LE LT GE GT
%token SEMI ASSIGN BANG BAR ADD SUB MULT MOD TRUE FALSE
%token COMMA DOT COLON    /* unused */
%token <string> IDENT
%token <int> INTLIT

%left AND /* OR */
%left SUB ADD
%left MULT MOD
%nonassoc BANG  /* NOT */

%%
