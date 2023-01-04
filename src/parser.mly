%{

%}

%token LPAREN RPAREN
%token UNIT
%token <bool> BOOL
%token <int> INT
%token <string> ID
%token EOF

%type <Sexpr.sexpr option> parse
%type <Sexpr.sexpr>        sexpr
%type <Sexpr.atom>        atom
%type <Sexpr.sexpr list>   slist
%type <Sexpr.sexpr list>   sexpr_list

%%


parse:
    | EOF { None }
    | s = sexpr { Some s }

sexpr:
    | a = atom { SExprAtom a }
    | l = slist { SExprList l }

atom:
    | UNIT { AtomUnit }
    | b = BOOL { AtomBool b }
    | i = INT { AtomInt i }
    | s = ID { AtomIdent s }

slist:
    | LPAREN l = sexpr_list RPAREN { l }

sexpr_list:
    | l = list(sexpr) { l }
