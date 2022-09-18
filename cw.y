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
%token IF_KEYWORD ELSE_KEYWORD SWITCH_KEYWORD CASE_KEYWORD DEFAULT_KEYWORD CHAN_KEYWORD LEFT_ARROW SELECT_KEYWORD
%token EQ_RELATION GREATER_RELATION LESS_RELATION EQ_GREATER_RELATION EQ_LESS_RELATION NOT_EQ_RELATION
%token FOR_KEYWORD BREAK_KEYWORD CONTINUE_KEYWORD RANGE_KEYWORD GO_KEYWORD GOTO_KEYWORD  FALL_KEYWORD
%token INT_TYPE FLOAT_TYPE  COMPLEX_TYPE  BOOL_TYPE STRING_TYPE

%left LEFT_ARROW
%left LOWER_THAN_RELATION
%left OR_OPERATION
%left AND_OPERATION
%left NOT_OPERATION
%left EQ_RELATION 
%left GREATER_RELATION 
%left LESS_RELATION 
%left EQ_GREATER_RELATION 
%left EQ_LESS_RELATION 
%left NOT_EQ_RELATION
%left '-' '+' '%'
%left '*' '/' '|'
%left '^' '&'
%left SHIFT_LEFT SHIFT_RIGHT

%nonassoc INCREMENT DECREMENT


%%


S :  
  PACKAGE  ';' GLOBALS 
  | PACKAGE  ';'
  ;



PACKAGE : 
  PACKAGE_KEYWORD IDENTIFICATOR                            { print("Package declaration"); } ;    


IMPORT : IMPORT_KEYWORD IMPORT_SPEC                            { print("Single module imported"); }
  | IMPORT_KEYWORD '('   ')'                                { print("Importing modules with brackets"); }
  | IMPORT_KEYWORD '('  IMPORT_MULTIPLE_SPECS ')'          { print("Importing modules with brackets"); }
  ;

IMPORT_SPEC:  
  STRING                        { print("Module path"); }
  | IDENTIFICATOR STRING        { print("Module path with identificator"); }
  | '.' STRING                  { print("Module path with dot"); }
  ;

IMPORT_MULTIPLE_SPECS :   
  IMPORT_SPEC ';'                          
  | IMPORT_MULTIPLE_SPECS IMPORT_SPEC ';'  
  ;



GLOBALS : 
  GLOBAL
  | GLOBALS GLOBAL
  ;

GLOBAL:
  FUNCTION ';'
  | IMPORT ';'
  | TYPEDECL ';'
  | DECLARATION    ';'
  ;

TYPEDECL:
  TYPE_KEYWORD TYPESPEC
  | TYPE_KEYWORD '(' ')'
  | TYPE_KEYWORD '(' MULTIPLE_TYPESPEC ')'

  ;

MULTIPLE_TYPESPEC:
  TYPESPEC ';'
  | MULTIPLE_TYPESPEC TYPESPEC ';'
  ;


TYPESPEC:
  IDENTIFICATOR TYPE          { print("Extra type definition"); } 
  | IDENTIFICATOR '=' TYPE     { print("Alyas definition"); } 
  ;


INTERFACE_FIELDS:
  INTERFACE_FIELD ';'
  | INTERFACE_FIELDS INTERFACE_FIELD ';'
  ;

INTERFACE_FIELD: 
  IDENTIFICATOR '('  ')'  { print("Interface method "); }
  | IDENTIFICATOR '('  ')' FUNC_RESULT { print("Interface method "); }
  | IDENTIFICATOR '(' INTERFACE_METHOD_ARGS')'  { print("Interface method "); }
  | IDENTIFICATOR '(' INTERFACE_METHOD_ARGS')' FUNC_RESULT { print("Interface method "); }
  | IDENTIFICATOR { print("Inner interface "); }
  ;

INTERFACE_METHOD_ARGS:
  TYPEVAL
  | INTERFACE_METHOD_ARGS ',' TYPEVAL

  ;

STRUCT_FIELDS:
  STRUCT_FIELD ';'                         { print("Struct field");}
  | STRUCT_FIELDS  STRUCT_FIELD ';'       { print("Struct field");}
  ;

STRUCT_FIELD: 
  MULTIPLE_IDENT TYPE            //{ print("Multiple ident type"); }
  | STRUCT_EMBEDDED_FIELD    
  | MULTIPLE_IDENT TYPE STRING
  | STRUCT_EMBEDDED_FIELD  STRING
  ;

STRUCT_EMBEDDED_FIELD:
  TypeName  
  //| TypeName  TypeArgs              //REDUCE/REDUCE
  | '*'  TypeName  
  //| '*'  TypeName  TypeArgs 

  ;

TypeName  : 
  FULL_IDENTIFICATOR 
  //| QUALIFIED_IDENT    // SHIFT/REDUCE
  ; 
TypeArgs  : 
  '[' MULTIPLE_TYPE  ']' 
  | '[' MULTIPLE_TYPE ',' ']' 
  ;

MULTIPLE_TYPE:
  TYPE
  | MULTIPLE_TYPE ',' TYPE
  ;

QUALIFIED_IDENT: IDENTIFICATOR '.' IDENTIFICATOR ;



FUNCTION : FUNC_KEYWORD IDENTIFICATOR SIGNATURE BLOCK { print("Function declaration"); }
  | FUNC_KEYWORD '(' IDENTIFICATOR TYPE ')' IDENTIFICATOR SIGNATURE BLOCK { print("Method declaration"); }//QUESTION 
  ;


ANON_FUNCTION: FUNC_KEYWORD  SIGNATURE  BLOCK  ;

SIGNATURE:
  '(' FUNC_PARAMETER_GROUPS ')' FUNC_RESULT 
  | '('  ')' FUNC_RESULT 
  | '(' FUNC_PARAMETER_GROUPS ')' 
  | '('  ')' 
  ;


FUNC_PARAMETER_GROUP :  
  MULTIPLE_IDENT TYPE { print("Group of parameters"); }
  | IDENTIFICATOR DOT_DOT_DOT TYPE { print("Dot dot dot parameter"); }
  ;


FUNC_PARAMETER_GROUPS : FUNC_PARAMETER_GROUP 
  | FUNC_PARAMETER_GROUPS ',' FUNC_PARAMETER_GROUP
  ;

FUNC_RESULT : 
  TYPE                { print("Single unnamed function result"); }

  | '(' FUNC_RESULT_NAMED ')'       { print("Multiple named function result"); }
  | '(' FUNC_RESULT_UNNAMED ')'     { print("Multiple unnamed function result"); }
  | '(' ')'
  ;

FUNC_RESULT_UNNAMED : 
  TYPE          { print("Type in unnamed function result"); }
  | FUNC_RESULT_UNNAMED ',' TYPE    { print("Type in unnamed function result"); }
  ;

FUNC_RESULT_NAMED :
  IDENTIFICATOR TYPE        { print("Type in named function result"); }//QUESTION
  | FUNC_RESULT_NAMED ',' IDENTIFICATOR TYPE  { print("Type in named function result"); }//QUESTION

  ;

BLOCK : 
  '{' STATEMENT_LIST '}' 
  | '{' STATEMENT '}'                   // ; may be ommited in complex lines
  ;
STATEMENT_LIST:
  STATEMENTS
  |
  ;


STATEMENTS : STATEMENT ';'
  | STATEMENTS  STATEMENT ';' 
  ;

STATEMENT : 
  DECLARATION  
  | IF_ELSE_STATEMENT 
  | LABELED_STMT
  | SWITCH 
  //| SELECT
  | SIMPLE_STATEMENT
  | FOR 
  | FALL_KEYWORD
  | GOTO_KEYWORD IDENTIFICATOR
  | BLOCK
  | BREAK
  | CONTINUE
  | RETURN 
  | DEFER_KEYWORD EXPRESSION  { print("Defer function call");}
  | GO_KEYWORD  EXPRESSION     { print("Starting goroutine"); }
  ;

LABELED_STMT: IDENTIFICATOR ':' STATEMENT ;

SIMPLE_STATEMENT:
  EXPRESSION
  | SEND_STMT
  | INC_DEC_STMT
  | Assignment
  | SHORT_DEFINING
  ;

INC_DEC_STMT: EXPRESSION INCREMENT | EXPRESSION DECREMENT ;
SEND_STMT: EXPRESSION LEFT_ARROW EXPRESSION ;


RETURN :
  RETURN_KEYWORD
  | RETURN_KEYWORD ExpressionList
  ;

BREAK:
  BREAK_KEYWORD
  | BREAK_KEYWORD ExpressionList
  ;

CONTINUE:
  CONTINUE_KEYWORD
  | CONTINUE_KEYWORD ExpressionList
  ;

UNARY_OPERATION : 
  IDENTIFICATOR INCREMENT
  | IDENTIFICATOR DECREMENT
  | INCREMENT IDENTIFICATOR
  | DECREMENT IDENTIFICATOR 
  ;

SELECT : SELECT_KEYWORD '{' '}' | SELECT_KEYWORD '{' RepeatingCommClause '}' ;
RepeatingCommClause: CommClause | RepeatingCommClause CommClause ;
CommClause : CommCase ':' STATEMENT_LIST ;
CommCase   : CASE_KEYWORD SEND_STMT| CASE_KEYWORD  RecvStmt| DEFAULT_KEYWORD ;
RecvStmt   : ExpressionList '=' RecvExpr | SIMPLE_IDENT_LIST COLON_EQ RecvExpr ;
RecvExpr   : EXPRESSION ;


FOR :
  FOR_KEYWORD FOR_CLAUSE BLOCK { print("For-loop"); }
  | FOR_KEYWORD  FOR_CONDITION BLOCK                         { print("Shortened for-loop"); }
  | FOR_KEYWORD FOR_RANGE BLOCK { print("For in range loop"); }
  ;

FOR_CLAUSE:
  ';' ';'
  | ';'  ';' SIMPLE_STATEMENT
  | ';' EXPRESSION ';' 
  | ';' EXPRESSION ';' SIMPLE_STATEMENT
  | SIMPLE_STATEMENT ';'  ';' 
  | SIMPLE_STATEMENT ';'  ';' SIMPLE_STATEMENT
  | SIMPLE_STATEMENT ';' EXPRESSION ';' 
  | SIMPLE_STATEMENT ';' EXPRESSION ';' SIMPLE_STATEMENT

FOR_CONDITION: EXPRESSION ;
FOR_RANGE:
  Assignment | //to prevent Reduce/reduce
  SIMPLE_IDENT_LIST COLON_EQ  RANGE_KEYWORD EXPRESSION
  ;


DECLARATION: 
  VAR_KEYWORD VARIABLE_DECLARATION                      { print("Simple variable declaration");                 }
  | VAR_KEYWORD VARIABLE_DECLARATION_ASSIGNMENT         { print("Declaration (with assignment) of variable");   }
  | VAR_KEYWORD '('  MULTIPLE_VARIABLE_DECLARATION  ')' { print("Multiple declaration");                        }
  | CONST_KEYWORD VARIABLE_DECLARATION_ASSIGNMENT       { print("Declaration (with assignment) of constant ");  }
  //| SHORT_DEFINING                                      { print("Short defining");                              }
  | VAR_KEYWORD SIMPLE_IDENT_LIST '=' EXPRESSION         
  ;


SWITCH :
  EXPRESSION_SWITCH

  ;

EXPRESSION_SWITCH:
  SWITCH_KEYWORD  '{'  '}'
  | SWITCH_KEYWORD OPT_EXPRESSION_SWITCH '{'  '}'
  | SWITCH_KEYWORD  '{' MULTIPLE_ExprCaseClause '}'
  | SWITCH_KEYWORD OPT_EXPRESSION_SWITCH '{' MULTIPLE_ExprCaseClause '}'
  ;

OPT_EXPRESSION_SWITCH:
  EXPRESSION
  | SIMPLE_STATEMENT ';' 
  | SIMPLE_STATEMENT ';' EXPRESSION
  ;


MULTIPLE_ExprCaseClause : ExprCaseClause | MULTIPLE_ExprCaseClause ExprCaseClause ;
ExprCaseClause : ExprSwitchCase ':' STATEMENT_LIST ;
ExprSwitchCase : CASE_KEYWORD ExpressionList | DEFAULT_KEYWORD ;


SWITCH_VALUES: 
 SWITCH_VALUES ',' RVALUE
 | RVALUE          
 ;

SWITCH_CASES:
  CASE_KEYWORD SWITCH_VALUES ':' STATEMENT_LIST   { print("Case in switch"); }
  | SWITCH_CASES  CASE_KEYWORD SWITCH_VALUES ':' STATEMENT_LIST { print("Case in switch"); }
  ;

IF_ELSE_STATEMENT: 
  IF_ELSE_IF
  | IF_ELSE_IF ELSE_KEYWORD BLOCK  { print("Else condition"); } 
  ;

IF_ELSE_IF:
  CONDITION                                              { print("Simple condition"); }  
  | IF_ELSE_IF ELSE_KEYWORD CONDITION           { print("Else-if condition"); }
  ;


CONDITION : 
  IF_KEYWORD EXPRESSION BLOCK 
  | IF_KEYWORD SIMPLE_STATEMENT ';' EXPRESSION BLOCK
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
  //| TYPE '{' INITIALIZER '}'                            

  //| '&' TYPE '{' INITIALIZER '}'         
  //| IDENTIFICATOR '{' INITIALIZER '}'                   
 // | TYPE '{' FUNCTION_CALL_ARGUMENTS '}'                
 // | IDENTIFICATOR '{' FUNCTION_CALL_ARGUMENTS '}'      
  | ANON_FUNCTION   
  | '(' RVALUE ')'
  //| '&' RVALUE
  ;

VALUE:
  BASIC_LITERAL
  | FUNCTION_CALL
  | FULL_IDENTIFICATOR
  | FULL_IDENTIFICATOR ARRAY_INDEXATION
  | '*' FULL_IDENTIFICATOR            { print("*FULL");}
  ;

Assignment : 
  EXPRESSION assign_op ExpressionList 
  | EXPRESSION assign_op RANGE_KEYWORD EXPRESSION 
  ;

assign_op : 
   '='
  | mul_op '='
  | add_op  '='
  ;


OPERAND:
  LITERAL
  | '(' EXPRESSION ')'
  | LITERAL_TYPE
  //| QUALIFIED_IDENT
  ;

BASIC_LITERAL: INTEGER | STRING | FLOAT | TRUE_FALSE  | COMPLEX | NIL_KEYWORD ;
LITERAL: BASIC_LITERAL | ANON_FUNCTION  | COMPOSITE_LITERAL;


COMPOSITE_LITERAL : LITERAL_TYPE LITERAL_VALUE ;

LITERAL_TYPE:
  STRUCT_TYPE
  | ARRAY_TYPE 
  | '[' DOT_DOT_DOT ']' TYPE
  | SLICE_TYPE
  | MAP_TYPE 
  | TypeName                                          // +5 REDUCE/REDUCE
  //| TypeName  TypeArgs  
  ;
LITERAL_VALUE : 
  '{'  '}' 
  | '{'  ElementList  '}' 
  | '{'  ElementList  ',' '}' 
  ;
ElementList   : 
  KeyedElement 
  | ElementList ',' KeyedElement
  ;
KeyedElement  : 
  Key ':' Element  |
   Element 
  ;
Key           : EXPRESSION | LITERAL_VALUE;
Element       : EXPRESSION | LITERAL_VALUE;


PrimaryExpr :
	OPERAND 
  //|	Conversion 
	| PrimaryExpr Selector 
	| PrimaryExpr Index 
	| PrimaryExpr Slice 
	| PrimaryExpr TypeAssertion 
	| PrimaryExpr Arguments     { print("PrimaryExpression Arguments"); }
  ;

Selector : '.' IDENTIFICATOR ;
Index : '[' EXPRESSION ']' ;
Slice : '['  ':'  ']' |
        '['  ':'  EXPRESSION  ']' |
        '['  EXPRESSION  ':'  ']' |
        '['  EXPRESSION  ':'  EXPRESSION  ']' |
        '['  ':' EXPRESSION ':' EXPRESSION ']' |
        '['  EXPRESSION  ':' EXPRESSION ':' EXPRESSION ']' 
      ;

TypeAssertion : '.' '(' TYPE ')' ;

ExpressionList:
  EXPRESSION
  | ExpressionList ',' EXPRESSION
  ;

Arguments  : 
  '(' ')'             
  | '('   ExpressionList  ')'             
  //| '('   TYPE    ')'
 // | '('   TYPE   ',' ExpressionList    ')'
  | '('   ExpressionList  ArgumentsAppendix ')'             
 //| '('   TYPE   ArgumentsAppendix ')'
  //| '('   TYPE   ',' ExpressionList   ArgumentsAppendix ')'
  ;

ArgumentsAppendix:
  DOT_DOT_DOT
  | ','
  | DOT_DOT_DOT ','
  ;

//MethodExpr: TYPE '.' IDENTIFICATOR ;


EXPRESSION : 
  UnaryExpr 
  | EXPRESSION binary_op EXPRESSION 
  ;
UnaryExpr  : 
  PrimaryExpr 
  | unary_op UnaryExpr 
  ;

binary_op  : 
  OR_OPERATION        
  | AND_OPERATION    
  | rel_op            
  | add_op           
  | mul_op          
  ;
rel_op     : EQ_RELATION | GREATER_RELATION | LESS_RELATION | EQ_GREATER_RELATION | EQ_LESS_RELATION | NOT_EQ_RELATION ;
mul_op     :  '/' | '%' | SHIFT_LEFT | SHIFT_RIGHT | '&';
add_op     : '+' | '-' | NOT_OPERATION | '^' ;
unary_op   : '+' | '-' | NOT_OPERATION | '^' | '*' | '&' | LEFT_ARROW;

//Conversion : Type '(' Expression  ')' | Type '(' Expression ',' ')' ;



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
  SIMPLE_IDENT_LIST COLON_EQ ExpressionList
  ;

SIMPLE_IDENT_LIST:
  IDENTIFICATOR
  | SIMPLE_IDENT_LIST ',' IDENTIFICATOR
  ;

MULTIPLE_VARIABLE_DECLARATION:
  MULTIPLE_VARIABLE_DECLARATION MULTIPLE_VARIABLE_DECL_OPTION ';'
  | MULTIPLE_VARIABLE_DECL_OPTION ';'
  ; 

MULTIPLE_VARIABLE_DECL_OPTION:
  VARIABLE_DECLARATION 
  | VARIABLE_DECLARATION_ASSIGNMENT
  | SIMPLE_IDENT_LIST '=' ExpressionList
  ;


VARIABLE_DECLARATION:
  SIMPLE_IDENT_LIST TYPE   
  ;

VARIABLE_DECLARATION_ASSIGNMENT: 
  SIMPLE_IDENT_LIST TYPE '='  ExpressionList
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
  TYPE_LIT
  | TypeName
  | '(' TYPE ')'
  ;

TYPE_LIT: 
  INT_TYPE
  | FLOAT_TYPE
  | COMPLEX_TYPE
  | BOOL_TYPE
  | STRING_TYPE
  | ARRAY_TYPE
  | '[' DOT_DOT_DOT ']' TYPE  
  | STRUCT_TYPE
  | POINTER_TYPE
  | FUNCTION_TYPE
  | INTERFACE_TYPE
  | SLICE_TYPE
  | MAP_TYPE
  //| CHANNEL_TYPE     // +8 Reduce/reduce
  ;

INTERFACE_TYPE:
  INTERFACE_KEYWORD '{'  '}'                    { print("Empty interface definition"); }
  | INTERFACE_KEYWORD '{' INTERFACE_FIELDS '}'    { print("Interface definition"); }
  ;
SLICE_TYPE :  '[' ']' TYPE ';'
CHANNEL_TYPE:
  CHAN_KEYWORD  TYPE 
  | CHAN_KEYWORD LEFT_ARROW  TYPE
  | LEFT_ARROW CHAN_KEYWORD  TYPE 
  ;

POINTER_TYPE : '*' TYPE;
FUNCTION_TYPE : FUNC_KEYWORD SIGNATURE ;
STRUCT_TYPE:
  STRUCT_KEYWORD '{' '}'                        { print("Empty struct definition"); }
  | STRUCT_KEYWORD '{' STRUCT_FIELD '}'           { print("One field struct definition"); }  // ; may be ommited in complex lines
  | STRUCT_KEYWORD '{' STRUCT_FIELDS '}'          { print("Struct definition"); }
  ;

ARRAY_TYPE : '[' EXPRESSION ']' TYPE ;
MAP_TYPE   : MAP_KEYWORD '[' TYPE ']' TYPE ;
MULTIPLE_IDENT: 
  FULL_IDENTIFICATOR 
  | MULTIPLE_IDENT ',' FULL_IDENTIFICATOR 
  ;



%%



void yyerror(char * msg) {
fprintf(stderr, "%s",  msg);

exit(1);
}