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

%token  IDENTIFICATOR COLON_EQ DOT_DOT_DOT NIL_KEYWORD DEFER_KEYWORD SHIFT_LEFT SHIFT_RIGHT OPER_ASSIGNMENT MAP_KEYWORD
%token STRING INTEGER FLOAT TRUE_FALSE COMPLEX
%token TYPE_KEYWORD STRUCT_KEYWORD INTERFACE_KEYWORD
%token CONST_KEYWORD PACKAGE_KEYWORD IMPORT_KEYWORD VAR_KEYWORD  FUNC_KEYWORD RETURN_KEYWORD
%token IF_KEYWORD ELSE_KEYWORD SWITCH_KEYWORD CASE_KEYWORD DEFAULT_KEYWORD
%token EQ_RELATION GREATER_RELATION LESS_RELATION EQ_GREATER_RELATION EQ_LESS_RELATION NOT_EQ_RELATION
%token FOR_KEYWORD BREAK_KEYWORD CONTINUE_KEYWORD RANGE_KEYWORD GO_KEYWORD
%token INT_TYPE FLOAT_TYPE  COMPLEX_TYPE  BOOL_TYPE STRING_TYPE


%left LOWER_THAN_RELATION
%left OR_OPERATION
%left AND_OPERATION
%left NOT_OPERATION
%left EQ_RELATION GREATER_RELATION LESS_RELATION EQ_GREATER_RELATION EQ_LESS_RELATION NOT_EQ_RELATION
%left '-' '+' '%'
%left '*' '/' '|'
%left '^' '&'
%left SHIFT_LEFT SHIFT_RIGHT

%nonassoc INCREMENT DECREMENT


%%

S :  BEFORE_PACKAGE PACKAGE  '\n' GLOBALS ;

BEFORE_PACKAGE:
  BEFORE_PACKAGE '\n'
  |
  ;

PACKAGE : 
  PACKAGE_KEYWORD IDENTIFICATOR                            { print("Package declaration"); } ;    

IMPORT : IMPORT_KEYWORD STRING                                  { print("Single module imported"); }
  | IMPORT_KEYWORD '(' '\n' IMPORT_MULTIPLE_STRING ')'          { print("Importing modules with brackets"); }
  ;

IMPORT_MULTIPLE_STRING :   
  IMPORT_MULTIPLE_FIELD  { print("Module name"); }
  | IMPORT_MULTIPLE_STRING IMPORT_MULTIPLE_FIELD { print("Module name"); }
  |
  ;
IMPORT_MULTIPLE_FIELD:
  STRING
  | '\n'
//  |
  ;
GLOBALS : 
  GLOBAL
  | GLOBALS '\n' GLOBAL
  ;

GLOBAL:
  FUNCTION
  | IMPORT
  | TYPEDEF
  | DECLARATION   
  | 
  ;


TYPEDEF:
  TYPE_KEYWORD TYPEVAL TYPEVAL     { print("Extra type definition"); }// QUESTION
 // | TYPE_KEYWORD IDENTIFICATOR STRUCT_KEYWORD '{' STRUCT_FIELDS '}' { print("Struct definition"); }
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
  TYPEVAL
  | INTERFACE_METHOD_ARGS ',' TYPEVAL
  |
  ;

STRUCT_FIELDS:
  STRUCT_FIELD '\n'
  |STRUCT_FIELDS  STRUCT_FIELD '\n'
  |
  ;

STRUCT_FIELD: 
  MULTIPLE_IDENT TYPEVAL 
  |
  ;

FUNCTION : FUNC_KEYWORD IDENTIFICATOR '(' FUNC_PARAMS ')' FUNC_RESULT  '{' FUNCTION_STATEMENTS '}' { print("Function declaration"); }
//  | FUNC_KEYWORD '(' TYPEVAL TYPEVAL ')' IDENTIFICATOR '(' FUNC_PARAMS ')' FUNC_RESULT  '{' FUNCTION_STATEMENTS '}' { print("Method declaration"); }//QUESTION
  | FUNC_KEYWORD '(' IDENTIFICATOR TYPE ')' IDENTIFICATOR '(' FUNC_PARAMS ')' FUNC_RESULT  '{' FUNCTION_STATEMENTS '}' { print("Method declaration"); }//QUESTION 
  ;

ANON_FUNCTION:
  FUNC_KEYWORD  '(' FUNC_PARAMS ')' FUNC_RESULT  '{' FUNCTION_STATEMENTS '}'
  ;


FUNCTION_STATEMENTS:
  STATEMENTS
  |
  ;

FUNC_PARAMETER_GROUP :  
  //MULTIPLE_IDENT TYPEVAL { print("Group of parameters"); }
  MULTIPLE_IDENT TYPE { print("Group of parameters"); }
  | MULTIPLE_IDENT FULL_IDENTIFICATOR { print("Group of parameters"); }
  | IDENTIFICATOR DOT_DOT_DOT TYPEVAL { print("Dot dot dot parameter"); }
  ;

FUNC_PARAMS: FUNC_PARAMETER_GROUPS
  |
  ;
FUNC_PARAMETER_GROUPS : FUNC_PARAMETER_GROUP 
  | FUNC_PARAMETER_GROUPS ',' FUNC_PARAMETER_GROUP
  ;

FUNC_RESULT : 
  TYPEVAL                 { print("Single unnamed function result"); }
  | '(' FUNC_RESULT_NAMED ')'       { print("Multiple named function result"); }
  | '(' FUNC_RESULT_UNNAMED ')'     { print("Multiple unnamed function result"); }
  |
  ;

FUNC_RESULT_UNNAMED : 
  TYPEVAL          { print("Type in unnamed function result"); }
  | FUNC_RESULT_UNNAMED ',' TYPEVAL    { print("Type in unnamed function result"); }
  |
  ;

FUNC_RESULT_NAMED :
  TYPEVAL TYPEVAL        { print("Type in named function result"); }//QUESTION
  | FUNC_RESULT_NAMED ',' TYPEVAL TYPEVAL  { print("Type in named function result"); }//QUESTION
  ;

STATEMENTS : STATEMENT 
  | STATEMENTS  STATEMENT 
  ;

STATEMENT : DECLARATION  
  | IF_ELSE_STATEMENT 
  | FUNCTION_CALL 
  | SWITCH 
  | ASSIGNMENT
  | UNARY_OPERATION 
  | FOR 
  | BREAK_KEYWORD 
  | CONTINUE_KEYWORD 
  | RETURN 
  | '\n'
  | DEFER_KEYWORD FUNCTION_CALL   { print("Defer function call");}
  | GO_KEYWORD  FUNCTION_CALL     { print("Starting goroutine"); }
  ;

RETURN :
  RETURN_KEYWORD
  | RETURN_KEYWORD MULTIPLE_RVALUE //REDUCE/REDUCE
  ;

UNARY_OPERATION : 
  IDENTIFICATOR INCREMENT
  | IDENTIFICATOR DECREMENT
  | INCREMENT IDENTIFICATOR
  | DECREMENT IDENTIFICATOR 
  ;

ASSIGNMENT : 
  MULTIPLE_IDENT '=' MULTIPLE_RVALUE   { print("Assignment of variable.");  }
  | FULL_IDENTIFICATOR ARRAY_INDEXATION  '=' RVALUE { print("Assigment of array element. "); }
  | '*' FULL_IDENTIFICATOR   '=' RVALUE { print("Assigment of pointer by address. "); } //
  | FULL_IDENTIFICATOR OPER_ASSIGNMENT RVALUE  { print("Operation + assignment");}
  ;
FOR :
  FOR_KEYWORD FOR_INIT ';' FOR_CONDITION ';' FOR_AFTER '{' STATEMENTS '}' { print("For-loop"); }
  | FOR_KEYWORD  FOR_CONDITION '{' STATEMENTS '}'                         { print("Shortened for-loop"); }
  | FOR_KEYWORD MULTIPLE_IDENT COLON_EQ RANGE_KEYWORD FULL_IDENTIFICATOR '{' STATEMENTS '}' { print("For in range loop"); }
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
  VAR_KEYWORD VARIABLE_DECLARATION                      { print("Simple variable declaration");                 }
  | VAR_KEYWORD VARIABLE_DECLARATION_ASSIGNMENT         { print("Declaration (with assignment) of variable");   }
  | VAR_KEYWORD '('  MULTIPLE_VARIABLE_DECLARATION  ')' { print("Multiple declaration");                        }
  | CONST_KEYWORD VARIABLE_DECLARATION_ASSIGNMENT       { print("Declaration (with assignment) of constant ");  }
  | SHORT_DEFINING                                      { print("Short defining");                              }         
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
  | IF_KEYWORD SHORT_DEFINING ';' LOGICAL_EXPRESSION '{' '}'
  | IF_KEYWORD SHORT_DEFINING ';' LOGICAL_EXPRESSION '{' STATEMENTS '}'
  | IF_KEYWORD ASSIGNMENT ';' LOGICAL_EXPRESSION '{' '}'
  | IF_KEYWORD ASSIGNMENT ';' LOGICAL_EXPRESSION '{' STATEMENTS '}'
  ;

LOGICAL_EXPRESSION :
  
    LOGICAL_EXPRESSION AND_OPERATION LOGICAL_EXPRESSION   { print("Logical AND"); }  
  | LOGICAL_EXPRESSION OR_OPERATION LOGICAL_EXPRESSION    { print("Logincal OR "); }  
  //| '(' LOGICAL_EXPRESSION ')'
  | RVALUE                  
  ;

RVALUE : 
  VALUE %prec LOWER_THAN_RELATION  
  | VALUE '+' RVALUE
  | VALUE '-' RVALUE
  | VALUE '*' RVALUE 
  | VALUE '/' RVALUE 
  | VALUE '%' RVALUE
  | VALUE '|' RVALUE
  | VALUE '^' RVALUE
  | VALUE '&' RVALUE
  | RVALUE GREATER_RELATION VALUE                          { print("Rvalues relation"); }  
  | RVALUE LESS_RELATION VALUE                           { print("Rvalues relation"); }  
  | RVALUE EQ_RELATION VALUE                             { print("Rvalues relation"); }  
  | RVALUE EQ_LESS_RELATION VALUE                        { print("Rvalues relation"); }  
  | RVALUE EQ_GREATER_RELATION VALUE                     { print("Rvalues relation"); }  
  | RVALUE NOT_EQ_RELATION VALUE                         { print("Rvalues nq relation"); }  
  | VALUE SHIFT_LEFT RVALUE
  | VALUE SHIFT_RIGHT RVALUE
  | NOT_OPERATION RVALUE                      { print("Denying expression"); }  
  | TYPE '{' INITIALIZER '}'                            
//  | '&' TYPE '{' INITIALIZER '}'                            
  | IDENTIFICATOR '{' INITIALIZER '}'                   
  | TYPE '{' FUNCTION_CALL_ARGUMENTS '}'                
  | IDENTIFICATOR '{' FUNCTION_CALL_ARGUMENTS '}'      
  | ANON_FUNCTION   
  | '(' RVALUE ')'
  | '&' RVALUE
  ;

VALUE:
  INTEGER
  | STRING
  | FLOAT
  | TRUE_FALSE
  | COMPLEX
  | NIL_KEYWORD
  | FUNCTION_CALL
  | FULL_IDENTIFICATOR
  | FULL_IDENTIFICATOR ARRAY_INDEXATION
//  | '&' FULL_IDENTIFICATOR            { print("Getting address of identificator");}
  | '*' FULL_IDENTIFICATOR            { print("*FULL");}
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
  TEST_CALL
  | FUNCTION_CALL '.' TEST_CALL
  ;
TEST_CALL :
  FULL_IDENTIFICATOR '(' FUNCTION_CALL_ARGUMENTS ')'   { print("Function call"); } 
  | ANON_FUNCTION      '(' FUNCTION_CALL_ARGUMENTS ')'   { print("Anon function call"); }
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
  MULTIPLE_IDENT COLON_EQ MULTIPLE_RVALUE
  | VAR_KEYWORD MULTIPLE_IDENT '=' MULTIPLE_RVALUE
  ;

MULTIPLE_VARIABLE_DECLARATION:
  VARIABLE_DECLARATION 
  | VARIABLE_DECLARATION_ASSIGNMENT
  | IDENTIFICATOR '=' MULTIPLE_RVALUE
  | MULTIPLE_VARIABLE_DECLARATION  VARIABLE_DECLARATION
  | MULTIPLE_VARIABLE_DECLARATION  VARIABLE_DECLARATION_ASSIGNMENT 
  | MULTIPLE_VARIABLE_DECLARATION  IDENTIFICATOR '=' MULTIPLE_RVALUE
  | MULTIPLE_VARIABLE_DECLARATION '\n'
  | '\n'
  ; 

VARIABLE_DECLARATION:
  MULTIPLE_IDENT TYPEVAL   
  ;

VARIABLE_DECLARATION_ASSIGNMENT: 
  MULTIPLE_IDENT TYPEVAL '='  MULTIPLE_RVALUE
  ;

MULTIPLE_RVALUE:
  MULTIPLE_RVALUE ',' RVALUE
  | RVALUE
  ;


TYPEVAL:
  TYPE
//  | IDENTIFICATOR
  ;


TYPE: 
  INT_TYPE
  | FLOAT_TYPE
  | COMPLEX_TYPE
  | BOOL_TYPE
  | STRING_TYPE
  | MAP_KEYWORD '[' TYPE ']' TYPE
  | FUNC_KEYWORD '(' FUNC_RESULT_UNNAMED ')' TYPE 
  | '[' INTEGER ']' TYPE
  | '[' DOT_DOT_DOT ']' TYPE
  | '[' ']' TYPE
  | '*' TYPE //Создает конфликт свептка/свертка с ???
  | FULL_IDENTIFICATOR
  | STRUCT_KEYWORD '{' STRUCT_FIELDS '}' { print("Struct definition"); }
  ;

MULTIPLE_IDENT: 
  FULL_IDENTIFICATOR 
  | MULTIPLE_IDENT ',' FULL_IDENTIFICATOR 
  ;



%%



void yyerror(char * msg) {
fprintf(stderr, "%s",  msg);

exit(1);
}
