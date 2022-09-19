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

%token IDENTIFICATOR COLON_EQ DOT_DOT_DOT DEFER_KEYWORD SHIFT_LEFT SHIFT_RIGHT OPER_ASSIGNMENT MAP_KEYWORD
%token STRING INTEGER FLOAT TRUE_FALSE COMPLEX
%token TYPE_KEYWORD STRUCT_KEYWORD INTERFACE_KEYWORD
%token CONST_KEYWORD PACKAGE_KEYWORD IMPORT_KEYWORD VAR_KEYWORD  FUNC_KEYWORD RETURN_KEYWORD 
%token IF_KEYWORD ELSE_KEYWORD SWITCH_KEYWORD CASE_KEYWORD DEFAULT_KEYWORD CHAN_KEYWORD LEFT_ARROW SELECT_KEYWORD
%token EQ_RELATION GREATER_RELATION LESS_RELATION EQ_GREATER_RELATION EQ_LESS_RELATION NOT_EQ_RELATION
%token FOR_KEYWORD BREAK_KEYWORD CONTINUE_KEYWORD RANGE_KEYWORD GO_KEYWORD GOTO_KEYWORD  FALL_KEYWORD
//%token INT_TYPE FLOAT_TYPE  COMPLEX_TYPE  BOOL_TYPE STRING_TYPE

%right IDENTIFICATOR
%left '+' '-' 
%left '*' '/' '%' 
%left '&' '|' '^' 
%left SHIFT_LEFT SHIFT_RIGHT
%left NOT_OPERATION 
%right '=' 

//%nonassoc LOWER_THAN_RELATION
%left OR_OPERATION AND_OPERATION 
%left EQ_RELATION GREATER_RELATION  LESS_RELATION  EQ_GREATER_RELATION  EQ_LESS_RELATION  NOT_EQ_RELATION INCREMENT DECREMENT
%left '(' ')' '{' '}' '[' ']' ',' '.' ';' ':'


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
  TYPE
  | INTERFACE_METHOD_ARGS ',' TYPE

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
  FULL_IDENTIFICATOR  
  | '*'  FULL_IDENTIFICATOR  
  ;


FUNCTION : FUNC_KEYWORD IDENTIFICATOR SIGNATURE BLOCK { print("Function declaration"); }
  | FUNC_KEYWORD '(' IDENTIFICATOR TYPE ')' IDENTIFICATOR SIGNATURE BLOCK { print("Method declaration"); }//QUESTION 
  ;


ANON_FUNCTION: FUNC_KEYWORD  SIGNATURE  BLOCK  ;


SIGNATURE:
  SIGN_TST FUNC_RESULT
  | SIGN_TST 
  ;

SIGN_TST:
  '(' FUNC_PARAMETER_GROUPS ')' 
  | '('  ')'
  ;


FUNC_PARAMETER_GROUP :  
  MULTIPLE_IDENT TYPE { print("Group of parameters"); }
  | IDENTIFICATOR DOT_DOT_DOT TYPE { print("Dot dot dot parameter"); }
  ;


FUNC_PARAMETER_GROUPS : 
  FUNC_PARAMETER_GROUP 
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
  | RETURN_KEYWORD EXPRESSIONList
  ;

BREAK:
  BREAK_KEYWORD
  | BREAK_KEYWORD EXPRESSIONList
  ;

CONTINUE:
  CONTINUE_KEYWORD
  | CONTINUE_KEYWORD EXPRESSIONList
  ;

FOR :
  FOR_KEYWORD FOR_CLAUSE BLOCK       { print("For-loop"); }
  | FOR_KEYWORD  FOR_CONDITION BLOCK { print("Shortened for-loop"); }
  | FOR_KEYWORD FOR_RANGE BLOCK      { print("For in range loop"); }
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
  Assignment 
  | SIMPLE_IDENT_LIST COLON_EQ  RANGE_KEYWORD EXPRESSION_NO_LIT
  ;


DECLARATION: 
  VAR_KEYWORD VARIABLE_DECLARATION                      { print("Simple variable declaration");                 }
  | VAR_KEYWORD VARIABLE_DECLARATION_ASSIGNMENT         { print("Declaration (with assignment) of variable");   }
  | VAR_KEYWORD '('  MULTIPLE_VARIABLE_DECLARATION  ')' { print("Multiple declaration");                        }
  | CONST_KEYWORD VARIABLE_DECLARATION_ASSIGNMENT       { print("Declaration (with assignment) of constant ");  }
  //| SHORT_DEFINING                                      { print("Short defining");                              }
  | VAR_KEYWORD SIMPLE_IDENT_LIST '=' EXPRESSION         
  | CONST_DECL
  ;


CONST_DECL: 
  CONST_KEYWORD CONST_SPEC
  | CONST_KEYWORD '(' ')'
  | CONST_KEYWORD '(' MULTIPLE_CONST_SPEC ')'
  ; 

CONST_SPEC :
  SIMPLE_IDENT_LIST
  | SIMPLE_IDENT_LIST '=' EXPRESSIONList
  ;

MULTIPLE_CONST_SPEC :
  MULTIPLE_CONST_SPEC CONST_SPEC ';'
  | CONST_SPEC ';'
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
ExprSwitchCase : CASE_KEYWORD EXPRESSIONList | DEFAULT_KEYWORD ;


IF_ELSE_STATEMENT :
    IF_KEYWORD EXPRESSION_NO_LIT BLOCK                                                           { print("[]  IfStmt - if EXPRESSION BLOCK."); }
    | IF_KEYWORD SIMPLE_STATEMENT ';' EXPRESSION_NO_LIT BLOCK                                    { print("[]  IfStmt - if SIMPLE_STATEMENT EXPRESSION BLOCK."); }
    | IF_KEYWORD EXPRESSION_NO_LIT BLOCK ELSE_KEYWORD IF_ELSE_STATEMENT                          { print("[]  IfStmt - if EXPRESSION BLOCK else IfStmt."); }
    | IF_KEYWORD EXPRESSION_NO_LIT BLOCK ELSE_KEYWORD BLOCK                                      { print("[]  IfStmt - if EXPRESSION BLOCK else BLOCK."); }
    | IF_KEYWORD SIMPLE_STATEMENT ';' EXPRESSION_NO_LIT BLOCK ELSE_KEYWORD IF_ELSE_STATEMENT     { print("[]  IfStmt - if SIMPLE_STATEMENT EXPRESSION BLOCK else IfStmt."); }
    | IF_KEYWORD SIMPLE_STATEMENT ';' EXPRESSION_NO_LIT BLOCK ELSE_KEYWORD BLOCK                 { print("[]  IfStmt - if SIMPLE_STATEMENT EXPRESSION BLOCK else BLOCK."); }
    ;

Assignment : 
  EXPRESSION assign_op EXPRESSIONList 
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
  ;


BASIC_LITERAL: INTEGER | STRING | FLOAT | TRUE_FALSE  | COMPLEX;
LITERAL: BASIC_LITERAL | ANON_FUNCTION  ;//| COMPOSITE_LITERAL;


COMPOSITE_LITERAL : 
  LITERAL_TYPE LITERAL_VALUE 
  | '&' COMPOSITE_LITERAL
  | '*' COMPOSITE_LITERAL
  ;

LITERAL_TYPE:
  STRUCT_TYPE 
  | ARRAY_TYPE 
  | '[' DOT_DOT_DOT ']' TYPE
  | SLICE_TYPE 
  | MAP_TYPE 
  | FULL_IDENTIFICATOR //{print("?");}                                         // +5 REDUCE/REDUCE
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
	| PrimaryExpr Selector 
	| PrimaryExpr Index 
	| PrimaryExpr Slice 
	| PrimaryExpr TypeAssertion 
	| PrimaryExpr Arguments     { print("PrimaryEXPRESSION Arguments"); }
  ;

Selector : 
  '.' FULL_IDENTIFICATOR
  ; 

Index : '[' EXPRESSION ']' ;
Slice : '['  ':'  ']' |
        '['  ':'  EXPRESSION  ']' |
        '['  EXPRESSION  ':'  ']' |
        '['  EXPRESSION  ':'  EXPRESSION  ']' |
        '['  ':' EXPRESSION ':' EXPRESSION ']' |
        '['  EXPRESSION  ':' EXPRESSION ':' EXPRESSION ']' 
      ;

TypeAssertion : '.' '(' TYPE ')' ;

EXPRESSIONList:
  EXPRESSION
  | EXPRESSIONList ',' EXPRESSION
  ;

Arguments  : 
  '(' ')'             
  | '('   EXPRESSIONList  ')'             
  | '('   EXPRESSIONList  ArgumentsAppendix ')'             
  ;

ArgumentsAppendix:
  DOT_DOT_DOT
  | ','
  | DOT_DOT_DOT ','
  ;

EXPRESSION:
  EXPRESSION_NO_LIT
  | COMPOSITE_LITERAL
  ;

EXPRESSION_NO_LIT :
  EXPRESSION_NO_LIT binary_op UnaryExpr 
  | UnaryExpr 
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


FULL_IDENTIFICATOR:
  IDENTIFICATOR 
  | IDENTIFICATOR '.' FULL_IDENTIFICATOR
  | '(' FULL_IDENTIFICATOR ')'
  ;

SHORT_DEFINING:
  SIMPLE_IDENT_LIST COLON_EQ EXPRESSIONList 
  ;

SIMPLE_IDENT_LIST:
  FULL_IDENTIFICATOR
  | SIMPLE_IDENT_LIST ',' FULL_IDENTIFICATOR
  ;

MULTIPLE_VARIABLE_DECLARATION:
  MULTIPLE_VARIABLE_DECLARATION MULTIPLE_VARIABLE_DECL_OPTION ';'
  | MULTIPLE_VARIABLE_DECL_OPTION ';'
  ; 

MULTIPLE_VARIABLE_DECL_OPTION:
  VARIABLE_DECLARATION 
  | VARIABLE_DECLARATION_ASSIGNMENT
  | SIMPLE_IDENT_LIST '=' EXPRESSIONList
  ;


VARIABLE_DECLARATION:
  SIMPLE_IDENT_LIST TYPE   
  ;

VARIABLE_DECLARATION_ASSIGNMENT: 
  SIMPLE_IDENT_LIST TYPE '='  EXPRESSIONList
  ;


TYPE: 
  TYPE_LIT
  | FULL_IDENTIFICATOR
  | '(' TYPE ')'
  ;

TYPE_LIT: 
  ARRAY_TYPE
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
SLICE_TYPE :  
  '[' ']' TYPE ';'
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