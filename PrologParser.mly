%{
	open Printf
	open Types
%}

%token <int> Integer Exclamation
%token <float> Reals
%token <string> Ident
%token <string> IdVar
%token LParentheses RParentheses
%token Newline Comma Dot Semi
%token If

%start program
%start queries
%start actions

%type <Types.tree> program
%type <Types.tree list> queries
%type <Types.action> actions

%%

actions: Semi { More }
	   | Dot { Stop }
	   | error Newline { print_string "Parse Error\n"; flush stdout; Stop 	 }
;

queries: /* empty */ { [Empty] }
	   | Newline { [Empty] }
	   | query Newline { flush stdout; $1 }
	   | error Newline { print_string "Parse Error\n"; flush stdout; [Empty] 	 }
;

program: /* empty */ { Empty }
	   | Newline { Empty }
	   | clause Newline{ flush stdout; $1 }
	   | error Newline { print_string "Parse Error\n"; flush stdout; Empty }
;

clause: fact { $1 }
	  | rule { $1 }
;

fact: apred Dot { $1 }
;

rule: head If body Dot { Rule($1,$3) }
;

head: apred { $1 }
;

body: apred { [$1] }
	| apred Comma body { $1 :: $3 }
	| Exclamation { [Cut($1)] }
	| Exclamation Comma body { (Cut($1))::$3 }
;

apred: Ident LParentheses termlist RParentheses { APred($1,$3) }
;

term: aterm { $1 }
	| naterm { $1 }
;

aterm: Ident { Const($1) }
	 | IdVar { Var($1) }
	 | Integer { Int($1) }
	 | Reals { Real($1) }
;

naterm: apred { $1 }
;

termlist: term { [$1] }
		| term Comma termlist { $1 :: $3 }
;

query: body Dot { $1 }
;

%%