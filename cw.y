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

%token  IDENTIFICATOR COLON_EQ DOT_DOT_DOT NIL_KEYWORD DEFER_KEYWORD SHIFT_LEFT SHIFT_RIGHT OPER_ASSIGNMENT
%token STRING INTEGER FLOAT TRUE_FALSE COMPLEX
%token TYPE_KEYWORD STRUCT_KEYWORD INTERFACE_KEYWORD
%token CONST_KEYWORD PACKAGE_KEYWORD IMPORT_KEYWORD VAR_KEYWORD  FUNC_KEYWORD RETURN_KEYWORD
%token IF_KEYWORD ELSE_KEYWORD SWITCH_KEYWORD CASE_KEYWORD DEFAULT_KEYWORD
%token EQ_RELATION GREATER_RELATION LESS_RELATION EQ_GREATER_RELATION EQ_LESS_RELATION NOT_EQ_RELATION
%token FOR_KEYWORD BREAK_KEYWORD CONTINUE_KEYWORD RANGE_KEYWORD GO_KEYWORD
%token INT_TYPE FLOAT_TYPE  COMPLEX_TYPE  BOOL_TYPE STRING_TYPE



%left OR_OPERATION AND_OPERATION NOT_OPERATION 
%left EQ_RELATION GREATER_RELATION LESS_RELATION EQ_GREATER_RELATION EQ_LESS_RELATION NOT_EQ_RELATION

%left '-' '+' '%'
%left '*' '/' '|'
%left '^' '&'
%left SHIFT_LEFT SHIFT_RIGHT

%nonassoc INCREMENT DECREMENT
%nonassoc LOWER_THAN_BRACKET
%nonassoc '[' '(' '{'

%%

S :  PACKAGE  GLOBALS    
  ;

PACKAGE : 
  PACKAGE_KEYWORD IDENTIFICATOR  '\n' { print("Package declaration"); }
  ;

IMPORT : IMPORT_KEYWORD STRING  { print("Single module imported"); }
  | IMPORT_KEYWORD '(' '\n' IMPORT_MULTIPLE_STRING ')'  { print("Importing modules with brackets"); }
  ;

IMPORT_MULTIPLE_STRING :   STRING '\n' IMPORT_MULTIPLE_STRING { print("Module name"); }
  |
  ;

GLOBALS : 
  GLOBAL '\n'
  | GLOBAL'\n' GLOBALS
  ;

GLOBAL:
  | IMPORT  
  | TYPEDEF
  | FUNCTION
  | DECLARATION   { print("Global declaration"); } 
  ;

RVALUE : 
  INTEGER   
  | STRING
  | FLOAT 
  | TRUE_FALSE 
  | COMPLEX 
  | NIL_KEYWORD 
  | '(' RVALUE ')' 
  | RVALUE '+' RVALUE
  | RVALUE '-' RVALUE
  | RVALUE '*' RVALUE
  | RVALUE '/' RVALUE 
  | RVALUE '%' RVALUE
  | RVALUE '|' RVALUE
  | RVALUE '^' RVALUE
  | RVALUE '&' RVALUE
  | RVALUE EQ_RELATION RVALUE        { print("Eq relation"); }  
  | RVALUE GREATER_RELATION RVALUE        { print("GREATER relation"); }  
  | RVALUE LESS_RELATION RVALUE        { print("LESS relation"); }  
  | RVALUE EQ_GREATER_RELATION RVALUE        { print("Eq GREATERrelation"); }  
  | RVALUE EQ_LESS_RELATION RVALUE        { print("Eq LESS relation"); }  
  | RVALUE NOT_EQ_RELATION RVALUE        { print("NOT Eq relation"); }  
  | RVALUE AND_OPERATION RVALUE   { print("Logical AND"); }  
  | RVALUE OR_OPERATION RVALUE    { print("Logincal OR "); }  
  | NOT_OPERATION RVALUE          { print("Denying expression"); }  
  | RVALUE SHIFT_LEFT RVALUE
  | RVALUE SHIFT_RIGHT RVALUE
  | FULL_IDENTIFICATOR  %prec LOWER_THAN_BRACKET
  | FULL_IDENTIFICATOR ARRAY_INDEXATION
  | ADDRESS_INDEXATION IDENTIFICATOR
  | POINTER_INDEXATION IDENTIFICATOR
  | FUNCTION_CALL
  //| TYPE '{' INITIALIZER '}'                            { print("Initializer");}  
  ;

ASSIGNMENT : 
  FULL_IDENTIFICATOR '=' RVALUE                                 { print("Assignment of variable.");  }
  | FULL_IDENTIFICATOR ARRAY_INDEXATION  '=' RVALUE             { print("Assigment of array element. "); }
  | POINTER_INDEXATION FULL_IDENTIFICATOR   '=' RVALUE          { print("Assigment of pointer by address. "); } //
  | FULL_IDENTIFICATOR OPER_ASSIGNMENT RVALUE                   { print("Operation + assignment");}
  ;
STATEMENTS : STATEMENT 
  | STATEMENTS '\n' STATEMENT 
  ;
STATEMENT : 
  DECLARATION  
  | IF_ELSE_STATEMENT 
  | FUNCTION_CALL 
  | SWITCH 
 // | 
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

INITIALIZER:  // SHIFT/REDUCE
  INITIALIZER_PAIRS
  | MULTIPLE_RVALUE
  ;

INITIALIZER_PAIRS:
  IDENTIFICATOR ':' RVALUE
  |  IDENTIFICATOR ':' RVALUE  ',' INITIALIZER_PAIRS 
  ;



FUNCTION_CALL : 
  FULL_IDENTIFICATOR '(' FUNCTION_CALL_ARGUMENTS ')'   { print("Function call"); } 
  | ANON_FUNCTION      '(' FUNCTION_CALL_ARGUMENTS ')'   { print("Anon function call"); } //SHIFT/REDUCE
  ;

FUNCTION_CALL_ARGUMENTS:
  MULTIPLE_RVALUE     { print("Multiple rvalue in function call arguments");}
  |
  ;


FUNCTION : FUNC_KEYWORD IDENTIFICATOR '(' FUNC_PARAMS ')' FUNC_RESULT  '{' FUNCTION_STATEMENTS '}' { print("Function declaration"); }
  | FUNC_KEYWORD '(' IDENTIFICATOR IDENTIFICATOR ')' IDENTIFICATOR '(' FUNC_PARAMS ')' FUNC_RESULT  '{' FUNCTION_STATEMENTS '}' { print("Method declaration"); }
  ;

ANON_FUNCTION:
  FUNC_KEYWORD  '(' FUNC_PARAMS ')' FUNC_RESULT  '{' FUNCTION_STATEMENTS '}'
  ;


FUNCTION_STATEMENTS:
  STATEMENTS
  |
  ;

FUNC_PARAMETER_GROUP :  
  MULTIPLE_IDENT TYPE { print("Group of parameters"); }
  | IDENTIFICATOR DOT_DOT_DOT TYPE { print("Dot dot dot parameter"); }
  ;

FUNC_PARAMS: FUNC_PARAMETER_GROUPS
  |
  ;
FUNC_PARAMETER_GROUPS : FUNC_PARAMETER_GROUP 
  |  FUNC_PARAMETER_GROUP ',' FUNC_PARAMETER_GROUPS 
  ;

FUNC_RESULT : 
  TYPE                              { print("Single unnamed function result"); }
  | '(' FUNC_RESULT_NAMED ')'       { print("Multiple named function result"); }
  | '(' FUNC_RESULT_UNNAMED ')'     { print("Multiple unnamed function result"); }
  |
  ;

FUNC_RESULT_UNNAMED : 
  TYPE          { print("Type in unnamed function result"); }
  | FUNC_RESULT_UNNAMED ',' TYPE    { print("Type in unnamed function result"); }
  |
  ;

FUNC_RESULT_NAMED :
  IDENTIFICATOR TYPE        { print("Type in named function result"); }//QUESTION
  | FUNC_RESULT_NAMED ',' IDENTIFICATOR TYPE  { print("Type in named function result"); }//QUESTION
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

CONDITION : IF_KEYWORD RVALUE '{' STATEMENTS '}' 
  | IF_KEYWORD RVALUE '{' '}' 
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
  RVALUE    { print("Logical expression in For-loop condition "); }
  |                        { print("Empty logical expression in For-loop condition "); }
  ;

FOR_AFTER :
  STATEMENT
  |                   { print("Emptyness in For-loop after-block "); }
  ;





UNARY_OPERATION : 
  IDENTIFICATOR INCREMENT
  | IDENTIFICATOR DECREMENT
  | INCREMENT IDENTIFICATOR
  | DECREMENT IDENTIFICATOR 
  ;


DECLARATION: 
  VAR_KEYWORD VARIABLE_DECLARATION 
  | VAR_KEYWORD VARIABLE_DECLARATION_ASSIGNMENT  { print("Declaration (with assignment) of variable"); }
  | VAR_KEYWORD '('  MULTIPLE_VARIABLE_DECLARATION  ')' { print("Multiple declaration"); }
  | CONST_KEYWORD VARIABLE_DECLARATION_ASSIGNMENT { print("Declaration (with assignment) of constant "); }
  | SHORT_DEFINING { print("Short defining"); }  
  ;

SHORT_DEFINING:
  IDENTIFICATOR COLON_EQ RVALUE
  | VAR_KEYWORD IDENTIFICATOR '=' RVALUE
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
  MULTIPLE_IDENT TYPE  {print("Declaration of variable");}  
  ;

VARIABLE_DECLARATION_ASSIGNMENT: 
  MULTIPLE_IDENT TYPE '='  MULTIPLE_RVALUE
  ;



RETURN :
  RETURN_KEYWORD
  | RETURN_KEYWORD MULTIPLE_RVALUE 
  ;
MULTIPLE_RVALUE:
  MULTIPLE_RVALUE ','  RVALUE 
  | RVALUE
  ;

ARRAY_INDEXATION:
  '[' RVALUE ']'
  | '[' RVALUE ']' ARRAY_INDEXATION 
  ;

ADDRESS_INDEXATION:
  '&'
  | '&' ADDRESS_INDEXATION 
  ;


POINTER_INDEXATION:
  '*'
  | '*' POINTER_INDEXATION
  ;




TYPE:
  INT_TYPE
  | FLOAT_TYPE
  | COMPLEX_TYPE
  | BOOL_TYPE
  | STRING_TYPE
  //| FUNC_KEYWORD '(' FUNC_RESULT_UNNAMED ')' TYPE 
  | '[' INTEGER ']' TYPE            { print("Array type");  }
  | '[' ']' TYPE                    { print("Array type");  }
  | '*' TYPE                        { print("Pointer type");}
  | IDENTIFICATOR   
  ;


TYPEDEF:
  TYPE_KEYWORD IDENTIFICATOR TYPE   { print("Type definition"); }
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
  TYPE
  | INTERFACE_METHOD_ARGS ',' TYPE
  |
  ;

STRUCT_FIELDS:
  STRUCT_FIELDS  STRUCT_FIELD '\n'
  |
  ;

STRUCT_FIELD: 
  MULTIPLE_IDENT TYPE 
  |
  ;

MULTIPLE_IDENT:
  IDENTIFICATOR
  | IDENTIFICATOR ',' MULTIPLE_IDENT
  ;


FULL_IDENTIFICATOR:
  IDENTIFICATOR
  |  IDENTIFICATOR  '.' FULL_IDENTIFICATOR
  ;



%%



void yyerror(char * msg) {
fprintf(stderr, "%s",  msg);

exit(1);
}
