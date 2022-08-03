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

%token  IDENTIFICATOR COLON_EQ DOT_DOT_DOT 
%token STRING INTEGER FLOAT TRUE_FALSE COMPLEX
%token CONST_KEYWORD PACKAGE_KEYWORD IMPORT_KEYWORD VAR_KEYWORD TYPE_KEYWORD FUNC_KEYWORD IF_KEYWORD ELSE_KEYWORD
%token EQ_RELATION GREATER_RELATION LESS_RELATION EQ_GREATER_RELATION EQ_LESS_RELATION NOT_EQ_RELATION

%left OR_OPERATION
%left AND_OPERATION
%left NOT_OPERATION



%%

S : PACKAGE GLOBALS 
  
  ;

PACKAGE : PACKAGE_KEYWORD IDENTIFICATOR '\n'  { printf("Package declaration\n"); }
  ;

IMPORT : IMPORT_KEYWORD STRING  { printf("Single module imported\n"); }
  | IMPORT_KEYWORD '(' '\n' IMPORT_MULTIPLE_STRING ')'  { printf("Importing modules with brackets\n"); }
  ;

IMPORT_MULTIPLE_STRING :   STRING '\n' IMPORT_MULTIPLE_STRING { printf("Module name\n"); }
  |
  ;


GLOBALS : 
    FUNCTION  
  | IMPORT 
  | GLOBALS '\n' FUNCTION 
  | GLOBALS '\n' IMPORT
  | GLOBALS '\n'
  | 
  ;

FUNCTION : FUNC_KEYWORD IDENTIFICATOR '(' FUNC_PARAMS ')' FUNC_RESULT  '{' STATEMENTS '}' { printf("Function declaration\n"); }
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




STATEMENTS : STATEMENT 
  | STATEMENTS  STATEMENT   
  ;

STATEMENT : VAR_KEYWORD VARIABLE_DECLARATION 
  | VAR_KEYWORD VARIABLE_DECLARATION_ASSIGNMENT  { printf("Declaration (with assignment) of variable\n"); }
  | VAR_KEYWORD '(' '\n' MULTIPLE_VARIABLE_DECLARATION  ')' { printf("Multiple declaration\n"); }
  | CONST_KEYWORD VARIABLE_DECLARATION_ASSIGNMENT { printf("Declaration (with assignment) of constant \n"); }
  | SHORT_DEFINING { printf("Short defining\n"); }  
  | IF_ELSE_STATEMENT '\n'
  | FUNCTION_CALL '\n'
  | '\n'
  ;

IF_ELSE_STATEMENT: 
  IF_ELSE_IF
  | IF_ELSE_IF ELSE_KEYWORD '{' STATEMENTS '}'  { printf("Else condition\n"); }
  
  ;

IF_ELSE_IF:
  CONDITION                                              { printf("Simple condition\n"); }  
  | IF_ELSE_IF ELSE_KEYWORD CONDITION           { printf("Else-if condition\n"); }
  ;

CONDITION : IF_KEYWORD LOGICAL_EXPRESSION '{' STATEMENTS '}' 
  | IF_KEYWORD LOGICAL_EXPRESSION '{' '}' 
  ;
  


LOGICAL_EXPRESSION :
  RVALUE RELATION RVALUE        { printf("Rvalues relation\n"); }  
  | NOT_OPERATION LOGICAL_EXPRESSION      { printf("Denying expression\n"); }  
  | TRUE_FALSE                  { printf("True/False relation operand\n"); }  
  | FUNCTION_CALL               { printf("Function call in relation\n"); }  
  | LOGICAL_EXPRESSION AND_OPERATION LOGICAL_EXPRESSION   { printf("Logical AND\n"); }  
  | LOGICAL_EXPRESSION OR_OPERATION LOGICAL_EXPRESSION   { printf("Logincal OR \n"); }  
  | '(' LOGICAL_EXPRESSION ')'                    
  ;

RELATION : EQ_RELATION 
  | GREATER_RELATION 
  | LESS_RELATION 
  | EQ_GREATER_RELATION 
  | EQ_LESS_RELATION 
  | NOT_EQ_RELATION
  ;

RVALUE : 
  INTEGER
  | STRING
  | FLOAT
  | TRUE_FALSE
  | COMPLEX
  | FUNCTION_CALL
  ;

FUNCTION_CALL : IDENTIFICATOR '(' FUNCTION_CALL_ARGUMENTS ')'   { printf("Function call\n")} ;

FUNCTION_CALL_ARGUMENTS :
  FUNCTION_CALL_ARGUMENTS ',' RVALUE
  | RVALUE
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












