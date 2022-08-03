%{
#include <stdio.h>
#include <stdlib.h>

int yylex();
//extern int yylex();
void yyerror(char *msg);


%}

%union {
int num;
char letter;
}

%token END_OF_FILE IDENTIFICATOR COLON_EQ DOT_DOT_DOT 
%token STRING INTEGER FLOAT TRUE_FALSE COMPLEX
%token CONST_KEYWORD PACKAGE_KEYWORD IMPORT_KEYWORD VAR_KEYWORD TYPE_KEYWORD FUNC_KEYWORD



%%

S : PACKAGE FUNCTIONS 
  
  ;

PACKAGE : PACKAGE_KEYWORD IDENTIFICATOR '\n'  { printf("Package declaration\n"); }
  ;

IMPORT : IMPORT_KEYWORD STRING '\n'  { printf("Module imported\n"); }
  | IMPORT_KEYWORD '(' '\n' IMPORT_MULTIPLE_STRING ')' '\n' { printf("Importing modules with brackets\n"); }
  ;



IMPORT_MULTIPLE_STRING :   STRING '\n' IMPORT_MULTIPLE_STRING { printf("Module name\n"); }
  |
  ;


FUNCTIONS : FUNCTION '\n' FUNCTIONS
  |
  ;

FUNCTION : FUNC_KEYWORD IDENTIFICATOR '(' FUNC_PARAMS ')' FUNC_RESULT  '{' '}' { printf("Function declaration\n"); }
  ;

FUNC_PARAMETER_GROUP :  MULTIPLE_IDENT TYPE { printf("Group of parameters\n"); }
  | IDENTIFICATOR DOT_DOT_DOT TYPE { printf("Dot dot dot parameter\n"); }
  ;
FUNC_PARAMS : FUNC_PARAMETER_GROUP 
  | FUNC_PARAMS ',' FUNC_PARAMETER_GROUP
  | 
  ;

FUNC_RESULT : TYPE                  { printf("Single unnamed function result\n"); }
  | '(' FUNC_RESULT_NAMED ')'       { printf("Multiple named function result\n"); }
  | '(' FUNC_RESULT_UNNAMED ')'     { printf("Multiple unnamed function result\n"); }
  |
  ;


FUNC_RESULT_UNNAMED : TYPE          { printf("Type in unnamed function result\n"); }
  | FUNC_RESULT_UNNAMED ',' TYPE    { printf("Type in unnamed function result\n"); }
  ;

FUNC_RESULT_NAMED : IDENTIFICATOR TYPE        { printf("Type in named function result\n"); }
  | FUNC_RESULT_NAMED ',' IDENTIFICATOR TYPE  { printf("Type in named function result\n"); }
  ;


COMMANDS: COMMANDS VAR_KEYWORD VARIABLE_DECLARATION 
  | COMMANDS VAR_KEYWORD VARIABLE_DECLARATION_ASSIGNMENT  { printf("Declaration (with assignment) of variable\n"); }
  | COMMANDS VAR_KEYWORD '(' '\n' MULTIPLE_VARIABLE_DECLARATION  ')' { printf("Multiple declaration\n"); }
  | COMMANDS CONST_KEYWORD VARIABLE_DECLARATION_ASSIGNMENT { printf("Declaration (with assignment) of constant \n"); }
  | COMMANDS SHORT_DEFINING { printf("Short defining\n"); }
  | COMMANDS IMPORT 
  | COMMANDS '\n'
  | COMMANDS END_OF_FILE
  |
  ;

SHORT_DEFINING:
  IDENTIFICATOR COLON_EQ INTEGER '\n'
  | IDENTIFICATOR COLON_EQ STRING '\n'
  | IDENTIFICATOR COLON_EQ FLOAT '\n'
  | IDENTIFICATOR COLON_EQ TRUE_FALSE '\n'
  | IDENTIFICATOR COLON_EQ COMPLEX '\n'
  | VAR_KEYWORD IDENTIFICATOR '=' INTEGER '\n'
  | VAR_KEYWORD IDENTIFICATOR '=' STRING '\n'
  | VAR_KEYWORD IDENTIFICATOR '=' FLOAT '\n'
  | VAR_KEYWORD IDENTIFICATOR '=' TRUE_FALSE '\n'
  | VAR_KEYWORD IDENTIFICATOR '=' COMPLEX '\n'
  ;

MULTIPLE_VARIABLE_DECLARATION:
  VARIABLE_DECLARATION 
  | VARIABLE_DECLARATION_ASSIGNMENT
  | MULTIPLE_VARIABLE_DECLARATION  VARIABLE_DECLARATION
  | MULTIPLE_VARIABLE_DECLARATION  VARIABLE_DECLARATION_ASSIGNMENT 
  ; 


 
VARIABLE_DECLARATION:
  MULTIPLE_IDENT TYPE '\n' {printf("Declaration of variable\n");}  
  ;

VARIABLE_DECLARATION_ASSIGNMENT: 
  MULTIPLE_IDENT TYPE '='  INTEGER '\n'
  | MULTIPLE_IDENT TYPE '='  STRING '\n'
  |  MULTIPLE_IDENT TYPE '='  FLOAT '\n'
  |  MULTIPLE_IDENT TYPE '='  TRUE_FALSE '\n'
  |  MULTIPLE_IDENT TYPE '='  COMPLEX '\n'
  ;


TYPE: TYPE_KEYWORD 
  | FUNC_KEYWORD '(' FUNC_RESULT_UNNAMED ')' TYPE 
  ;

MULTIPLE_IDENT: IDENTIFICATOR 
  | MULTIPLE_IDENT ',' IDENTIFICATOR 
  ;



%%



void yyerror(char * msg) {
fprintf(stderr, "%s\n",  msg);

exit(1);
}












