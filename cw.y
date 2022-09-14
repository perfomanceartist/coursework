%{
#include <stdio.h>
#include <stdlib.h>

int yylex();
//extern int yylex();
void yyerror(char *msg);
extern void print(char *msg);

%}

%union {
int num;
char letter;
}

%token  IDENTIFICATOR COLON_EQ DOT_DOT_DOT 
%token STRING INTEGER FLOAT TRUE_FALSE COMPLEX
%token TYPE_KEYWORD STRUCT_KEYWORD INTERFACE_KEYWORD
%token CONST_KEYWORD PACKAGE_KEYWORD IMPORT_KEYWORD VAR_KEYWORD  FUNC_KEYWORD RETURN_KEYWORD
%token IF_KEYWORD ELSE_KEYWORD SWITCH_KEYWORD CASE_KEYWORD DEFAULT_KEYWORD
%token EQ_RELATION GREATER_RELATION LESS_RELATION EQ_GREATER_RELATION EQ_LESS_RELATION NOT_EQ_RELATION
%token FOR_KEYWORD BREAK_KEYWORD CONTINUE_KEYWORD RANGE_KEYWORD
%token INT_TYPE FLOAT_TYPE  COMPLEX_TYPE  BOOL_TYPE STRING_TYPE


%left OR_OPERATION
%left AND_OPERATION
%left NOT_OPERATION

%left '-' '+' '%'
%left '*' '/'

%nonassoc INCREMENT DECREMENT


%%

S : 
  PACKAGE GLOBALS 
  ;

PACKAGE : 
  PACKAGE_KEYWORD IDENTIFICATOR '\n'  { print("Package declaration"); }
  ;

IMPORT : IMPORT_KEYWORD STRING  { print("Single module imported"); }
  | IMPORT_KEYWORD '(' '\n' IMPORT_MULTIPLE_STRING ')'  { print("Importing modules with brackets"); }
  ;

IMPORT_MULTIPLE_STRING :   STRING '\n' IMPORT_MULTIPLE_STRING { print("Module name"); }
  |
  ;

GLOBALS : 
  FUNCTION  
  | IMPORT 
  | TYPEDEF
  | GLOBALS '\n' FUNCTION 
  | GLOBALS '\n' IMPORT
  | GLOBALS '\n' TYPEDEF
  | GLOBALS '\n'
  | 
  ;

TYPEDEF:
  TYPE_KEYWORD TESTVAL TESTVAL     { print("Extra type definition"); }// QUESTION
  | TYPE_KEYWORD IDENTIFICATOR STRUCT_KEYWORD '{' STRUCT_FIELDS '}' { print("Struct definition"); }
  | TYPE_KEYWORD IDENTIFICATOR INTERFACE_KEYWORD '{' INTERFACE_FIELDS '}' { print("Interface definition"); }
  ;

INTERFACE_FIELDS:
  INTERFACE_FIELDS INTERFACE_FIELD '\n'
  |
  ;

INTERFACE_FIELD: 
  IDENTIFICATOR '(' INTERFACE_METHOD_ARGS')' FUNC_RESULT { print("Interface method "); }
  | IDENTIFICATOR { print("Inner interface "); }
  |
  ;

INTERFACE_METHOD_ARGS:
  TESTVAL
//  TYPE
//  | IDENTIFICATOR
  | INTERFACE_METHOD_ARGS ',' TESTVAL
  |
  ;

STRUCT_FIELDS:
  STRUCT_FIELDS  STRUCT_FIELD '\n'
  |
  ;

STRUCT_FIELD: 
//  MULTIPLE_IDENT TYPE
//  | MULTIPLE_IDENT IDENTIFICATOR  
  MULTIPLE_IDENT TESTVAL 
  |
  ;

FUNCTION : FUNC_KEYWORD IDENTIFICATOR '(' FUNC_PARAMS ')' FUNC_RESULT  '{' STATEMENTS '}' { print("Function declaration"); }
  | FUNC_KEYWORD '(' TESTVAL TESTVAL ')' IDENTIFICATOR '(' FUNC_PARAMS ')' FUNC_RESULT  '{' STATEMENTS '}' { print("Method declaration"); }//QUESTION ?!?!??!?!??!
  ;

FUNC_PARAMETER_GROUP :  
  MULTIPLE_IDENT TESTVAL { print("Group of parameters"); }
  //MULTIPLE_IDENT TYPE { print("Group of parameters"); }
  //| MULTIPLE_IDENT IDENTIFICATOR { print("Group of parameters"); }
  //| IDENTIFICATOR DOT_DOT_DOT TYPE { print("Dot dot dot parameter"); }
  //| IDENTIFICATOR DOT_DOT_DOT IDENTIFICATOR { print("Dot dot dot parameter"); }
  | IDENTIFICATOR DOT_DOT_DOT TESTVAL { print("Dot dot dot parameter"); }
  ;

FUNC_PARAMS : FUNC_PARAMETER_GROUP 
  | FUNC_PARAMS ',' FUNC_PARAMETER_GROUP
  | 
  ;

FUNC_RESULT : 
  TESTVAL                 { print("Single unnamed function result"); }
//  TYPE                  { print("Single unnamed function result"); }
//  | IDENTIFICATOR                 { print("Single unnamed function result"); }
  | '(' FUNC_RESULT_NAMED ')'       { print("Multiple named function result"); }
  | '(' FUNC_RESULT_UNNAMED ')'     { print("Multiple unnamed function result"); }
  |
  ;

FUNC_RESULT_UNNAMED : 
  TESTVAL          { print("Type in unnamed function result"); }
//  TYPE          { print("Type in unnamed function result"); }
//  | IDENTIFICATOR
  | FUNC_RESULT_UNNAMED ',' TESTVAL    { print("Type in unnamed function result"); }
  ;

FUNC_RESULT_NAMED :
  TESTVAL TESTVAL        { print("Type in named function result"); }//QUESTION
  | FUNC_RESULT_NAMED ',' TESTVAL TESTVAL  { print("Type in named function result"); }//QUESTION
  ;

STATEMENTS : STATEMENT 
  | STATEMENTS  STATEMENT   
  ;

STATEMENT : DECLARATION '\n' 
  | IF_ELSE_STATEMENT '\n'
  | FUNCTION_CALL '\n'
  | SWITCH '\n'
  | ASSIGNMENT
  | UNARY_OPERATION '\n'
  | FOR '\n'
  | BREAK_KEYWORD '\n'
  | CONTINUE_KEYWORD '\n'
  | RETURN '\n'
  | '\n'
  ;

RETURN :
  RETURN_KEYWORD
  | RETURN_KEYWORD MULTIPLE_IDENT
//  | RETURN_KEYWORD RVALUE //REDUCE/REDUCE
  ;

UNARY_OPERATION : 
  IDENTIFICATOR INCREMENT
  | IDENTIFICATOR DECREMENT
  | INCREMENT IDENTIFICATOR
  | DECREMENT IDENTIFICATOR 
  ;

ASSIGNMENT : 
  IDENTIFICATOR '=' RVALUE   { print("Assignment of variable.");  }
  | IDENTIFICATOR ARRAY_INDEXATION  '=' RVALUE { print("Assigment of array element. "); }
  | '*' IDENTIFICATOR   '=' RVALUE { print("Assigment of pointer by address. "); } //
  
  ;
FOR :
  FOR_KEYWORD FOR_INIT ';' FOR_CONDITION ';' FOR_AFTER '{' STATEMENTS '}' { print("For-loop"); }
  | FOR_KEYWORD  FOR_CONDITION '{' STATEMENTS '}'                         { print("Shortened for-loop"); }
  | FOR_KEYWORD IDENTIFICATOR ',' IDENTIFICATOR COLON_EQ RANGE_KEYWORD IDENTIFICATOR '{' STATEMENTS '}' { print("For in range loop"); }
  ;

FOR_INIT : SHORT_DEFINING       { print("Short defining in For-loop init "); }
  | ASSIGNMENT                  { print("Assigment in For-loop init "); }
  |                             { print("Emptyness in For-loop init "); }
  ;

FOR_CONDITION :
  LOGICAL_EXPRESSION    { print("Logical expression in For-loop condition "); }
  |                        { print("Empty logical expression in For-loop condition "); }
  ;

FOR_AFTER :
  ASSIGNMENT          { print("Assignment in For-loop after-block "); }
  | FUNCTION_CALL     { print("Function call in For-loop after-block "); }
  | UNARY_OPERATION   { print("Unary operation in For-loop after-block "); }
  |                   { print("Emptyness in For-loop after-block "); }
  ;

DECLARATION: 
  VAR_KEYWORD VARIABLE_DECLARATION 
  | VAR_KEYWORD VARIABLE_DECLARATION_ASSIGNMENT  { print("Declaration (with assignment) of variable"); }
  | VAR_KEYWORD '(' '\n' MULTIPLE_VARIABLE_DECLARATION  ')' { print("Multiple declaration"); }
  | CONST_KEYWORD VARIABLE_DECLARATION_ASSIGNMENT { print("Declaration (with assignment) of constant "); }
  | SHORT_DEFINING { print("Short defining"); }  
  ;


SWITCH :
  SWITCH_KEYWORD '(' RVALUE ')' '{' '\n' SWITCH_CASES '}'
  | SWITCH_KEYWORD '(' RVALUE ')' '{' '\n' SWITCH_CASES DEFAULT_KEYWORD ':' STATEMENTS '}' { print("Default case in switch"); }
  ;

SWITCH_VALUES: 
 SWITCH_VALUES ',' RVALUE
 | RVALUE          
 ;

SWITCH_CASES:
  CASE_KEYWORD SWITCH_VALUES ':' STATEMENTS   { print("Case in switch"); }
  | SWITCH_CASES  CASE_KEYWORD SWITCH_VALUES ':' STATEMENTS { print("Case in switch"); }
  ;

IF_ELSE_STATEMENT: 
  IF_ELSE_IF
  | IF_ELSE_IF ELSE_KEYWORD '{' STATEMENTS '}'  { print("Else condition"); } 
  ;

IF_ELSE_IF:
  CONDITION                                              { print("Simple condition"); }  
  | IF_ELSE_IF ELSE_KEYWORD CONDITION           { print("Else-if condition"); }
  ;

CONDITION : IF_KEYWORD LOGICAL_EXPRESSION '{' STATEMENTS '}' 
  | IF_KEYWORD LOGICAL_EXPRESSION '{' '}' 
  ;

LOGICAL_EXPRESSION :
  RVALUE RELATION RVALUE        { print("Rvalues relation"); }  
  | NOT_OPERATION LOGICAL_EXPRESSION      { print("Denying expression"); }  
  | TRUE_FALSE                  { print("True/False relation operand"); }  
  | FUNCTION_CALL               { print("Function call in relation"); }  
  | LOGICAL_EXPRESSION AND_OPERATION LOGICAL_EXPRESSION   { print("Logical AND"); }  
  | LOGICAL_EXPRESSION OR_OPERATION LOGICAL_EXPRESSION   { print("Logincal OR "); }  
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
  VALUE
  | VALUE '+' RVALUE
  | VALUE '-' RVALUE
  | VALUE '*' RVALUE //Создает 2 конфликта сдвиг/свертка с   | FULL_IDENTIFICATOR и   | FULL_IDENTIFICATOR ARRAY_INDEXATION 
  | VALUE '/' RVALUE 
  | VALUE '%' RVALUE
//  | TESTVAL '{' INITIALIZER '}'                            { print("INITIALIZER_testVAL");}
  | TYPE '{' INITIALIZER '}'                            { print("INITIALIZER");}  
  | IDENTIFICATOR '{' INITIALIZER '}'                   { print("INITIALIZER2");} 
  | TYPE '{' FUNCTION_CALL_ARGUMENTS '}'                { print("INITIALIZER3");} 
  | IDENTIFICATOR '{' FUNCTION_CALL_ARGUMENTS '}'                { print("INITIALIZER4");} 
//  | TESTVAL '{' FUNCTION_CALL_ARGUMENTS '}'                { print("INITIALIZER_func");} 
  ;

VALUE:
  INTEGER
  | STRING
  | FLOAT
  | TRUE_FALSE
  | COMPLEX
  | FUNCTION_CALL
  | FULL_IDENTIFICATOR
  | FULL_IDENTIFICATOR ARRAY_INDEXATION
  | '&' FULL_IDENTIFICATOR                 { print("Getting address of identificator");}
  | '*' FULL_IDENTIFICATOR
  ;


INITIALIZER:
  IDENTIFICATOR ':' VALUE
  | INITIALIZER ',' IDENTIFICATOR ':' VALUE
  ;


ARRAY_INDEXATION:
  '[' VALUE ']'
  | ARRAY_INDEXATION '[' VALUE ']'
  ;


FUNCTION_CALL : 
  FULL_IDENTIFICATOR '(' FUNCTION_CALL_ARGUMENTS ')'   { print("Function call"); } 
  ;

FULL_IDENTIFICATOR:
  IDENTIFICATOR
  | FULL_IDENTIFICATOR '.' IDENTIFICATOR
  ;

FUNCTION_CALL_ARGUMENTS :
  FUNCTION_CALL_ARGUMENTS ',' RVALUE
  | RVALUE
  |
  ;

SHORT_DEFINING:
  IDENTIFICATOR COLON_EQ RVALUE
  | VAR_KEYWORD IDENTIFICATOR '=' RVALUE
  ;

MULTIPLE_VARIABLE_DECLARATION:
  VARIABLE_DECLARATION 
  | VARIABLE_DECLARATION_ASSIGNMENT
  | MULTIPLE_VARIABLE_DECLARATION  VARIABLE_DECLARATION
  | MULTIPLE_VARIABLE_DECLARATION  VARIABLE_DECLARATION_ASSIGNMENT 
  ; 


 
VARIABLE_DECLARATION:
  MULTIPLE_IDENT TESTVAL  {print("Declaration of variable");}  
//  MULTIPLE_IDENT TYPE  {print("Declaration of variable");}  
//  | MULTIPLE_IDENT IDENTIFICATOR  {print("Declaration of variable");}  
  ;

VARIABLE_DECLARATION_ASSIGNMENT: 
  MULTIPLE_IDENT TESTVAL '='  RVALUE
//  MULTIPLE_IDENT TYPE '='  RVALUE
//  | MULTIPLE_IDENT IDENTIFICATOR '='  RVALUE
  ;

TESTVAL:
  TYPE
  | IDENTIFICATOR
  ;


TYPE: 
  INT_TYPE
  | FLOAT_TYPE
  | COMPLEX_TYPE
  | BOOL_TYPE
  | STRING_TYPE
  | FUNC_KEYWORD '(' FUNC_RESULT_UNNAMED ')' TYPE 
  | '[' INTEGER ']' TYPE
  | '[' DOT_DOT_DOT ']' TYPE
  | '*' TYPE //Создает конфликт свептка/свертка с ???
//  | IDENTIFICATOR
  ;

MULTIPLE_IDENT: 
  IDENTIFICATOR 
  | MULTIPLE_IDENT ',' IDENTIFICATOR 
  ;



%%



void yyerror(char * msg) {
fprintf(stderr, "%s",  msg);

exit(1);
}
