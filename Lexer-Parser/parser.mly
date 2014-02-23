%{
    open Printf
%}

/* Token */
%token <string> ID
%token PROGRAM BEGIN END SEMICOLON DOT

/* Start du parser */
%start program
%type <unit> program


%%

/* Ajout de l'initialisation des variables PASCAL */


/* Structure principale d'un programme PASCAL */
program:
	PROGRAM i = ID SEMICOLON
	BEGIN
	END
	DOT
	{ printf "program %s;\n begin\n end.\n\n" i}

%%