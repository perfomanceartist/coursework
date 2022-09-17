%{
#include <stdio.h>
#include <stdlib.h>

int yylex();
extern int line_number;
void yyerror(char *msg);
extern void print(char *msg);

%}

%token identifier
%token BREAK_KEYWORD CASE_KEYWORD  CHAN_KEYWORD CONST_KEYWORD CONTINUE_KEYWORD DEFAULT_KEYWORD DEFER_KEYWORD ELSE_KEYWORD
%token FALL_KEYWORD FOR_KEYWORD FUNC_KEYWORD GO_KEYWORD GOTO_KEYWORD IF_KEYWORD IMPORT_KEYWORD INTERFACE_KEYWORD
%token MAP_KEYWORD PACKAGE_KEYWORD RANGE_KEYWORD RETURN_KEYWORD
%token SELECT_KEYWORD STRUCT_KEYWORD SWITCH_KEYWORD TYPE_KEYWORD VAR_KEYWORD

%token INT_TYPE FLOAT_TYPE COMPLEX_TYPE BOOL_TYPE STRING_TYPE

%nonassoc PREC_1
%nonassoc PREC_2
%nonassoc PREC_3
%nonassoc PREC_4
%nonassoc PREC_5

%token int_lit float_lit  bool_lit imaginary_lit string_lit
%token COLON_EQ DOT_DOT_DOT LEFT_ARROW SHIFT NIL_KEYWORD
%left LOGICAL_OR
%left LOGICAL_AND
%left '!'
%left EQ_RELATION GREATER_RELATION LESS_RELATION GREATER_EQ_RELATION LESS_EQ_RELATION NOT_EQ_RELATION
%left INCREMENT DECREMENT
%left '-' '+' '%'
%left '*' '/' '|'
%left '^' '&'
%left SHIFT



%%

S: SourceFile;


SourceFile : 
  PackageClause ';'  RepeatingTopLevelDecl                        |
  PackageClause ';'                                               |
  PackageClause ';'  RepeatingImportDecl                          |
  PackageClause ';'  RepeatingImportDecl  RepeatingTopLevelDecl   ;
RepeatingImportDecl: ImportDecl ';' | RepeatingImportDecl ImportDecl ';' ;
RepeatingTopLevelDecl: TopLevelDecl ';' | RepeatingTopLevelDecl TopLevelDecl ';' ;
PackageClause  : PACKAGE_KEYWORD PackageName { print("Package Clause"); };
PackageName    : identifier ;

ImportDecl       : 
  IMPORT_KEYWORD ImportSpec { print("Import declaration"); } 
  | IMPORT_KEYWORD  '(' ')'  { print("Import declaration"); }
  | IMPORT_KEYWORD  '(' RepeatingImportSpec ')'  { print("Import declaration"); } 
  ;
RepeatingImportSpec: 
  ImportSpec ';' 
  | RepeatingImportSpec ImportSpec ';' 
  ;
ImportSpec       :  
  '.'  ImportPath             {print("ImportSpec");}
  |  PackageName ImportPath   {print("ImportSpec");}
  | ImportPath                {print("ImportSpec");}
  ;
ImportPath       : string_lit { print("Import path"); };


Declaration : 
  ConstDecl 
  | TypeDecl 
  | VarDecl 
  ;

TopLevelDecl: 
  Declaration 
  | FunctionDecl        { print("Function declaration"); }
  | MethodDecl 
  ;

ConstDecl:
  CONST_KEYWORD  ConstSpec                { print("Const declaration"); }
  | CONST_KEYWORD '('  RepeatingConstSpec  ')'  { print("Const declaration (in brackets)");}
  | CONST_KEYWORD '('  ')'                { print("Empty const declaration");}
  ;

RepeatingConstSpec:
  ConstSpec ';'                           { print("ConstSpec"); }
  | RepeatingConstSpec ConstSpec ';'      { print("ConstSpec"); }
  ;


ConstSpec:
 IdentifierList   
 | IdentifierList OptConstSpecFields
 ;

OptConstSpecFields: 
  '=' ExpressionList
  |  Type  '=' ExpressionList
  ;
  
TypeDecl:
  TYPE_KEYWORD TypeSpec 
  | TYPE_KEYWORD  '(' RepeatingTypeSpec ')'
  | TYPE_KEYWORD  '('  ')'
  ;

TypeDef: 
  identifier  Type 
 | identifier  TypeParameters  Type 
 ;


RepeatingTypeSpec:
  TypeSpec ';'
  | RepeatingTypeSpec TypeSpec ';'
  ;

TypeSpec: AliasDecl | TypeDef  ;

AliasDecl: identifier '=' Type ;

TypeParameters:  '[' TypeParamList  ']' | '[' TypeParamList ',' ']'  ;

TypeParamList: RepeatingTypeParamDecl ;


RepeatingTypeParamDecl: RepeatingTypeParamDecl ',' TypeParamDecl | TypeParamDecl  ;

TypeParamDecl: IdentifierList TypeConstraint ;

TypeConstraint: TypeElem ;

IdentifierList : identifier | IdentifierList ',' identifier;
 
ExpressionList: Expression | ExpressionList ',' Expression ;

VarDecl: 
  VAR_KEYWORD  '(' RepeatingVarSpec')'
  | VAR_KEYWORD '(' ')'
  | VAR_KEYWORD  VarSpec
  ;
RepeatingVarSpec: VarSpec ';' | RepeatingVarSpec VarSpec ';' ;

VarSpec: 
  IdentifierList  '=' ExpressionList 
  | IdentifierList  Type 
  | IdentifierList  Type  '=' ExpressionList 
  ;

ShortVarDecl: IdentifierList COLON_EQ ExpressionList  { print("ShortVarDecl"); };

FunctionDecl:
  FUNC_KEYWORD FunctionName Signature 
  | FUNC_KEYWORD FunctionName Signature  FunctionBody 
  | FUNC_KEYWORD FunctionName TypeParameters Signature 
  | FUNC_KEYWORD FunctionName TypeParameters Signature  FunctionBody 
  ;
FunctionName: identifier ;
FunctionBody: Block ;

MethodDecl: 
  FUNC_KEYWORD Receiver MethodName Signature 
  FUNC_KEYWORD Receiver MethodName Signature  FunctionBody 

Receiver: Parameters ;

FunctionType: FUNC_KEYWORD Signature ;

Signature:
  Parameters 
  | Parameters  Result 
  ;
Result : 
  Parameters                            { print("Parameters as a result"); }
  | Type                                { print("Type as a result"); }
  ;
Parameters:
   '('  ')'                             { print("Empty function parameters"); }
   | '(' ParameterList ')'              { print("Parameters list"); }
   | '(' ParameterList ','   ')'        { print("Parameters list ,"); }
   ;
ParameterList:
  ParameterDecl
  | ParameterList ',' ParameterDecl 
  ;
ParameterDecl:
  Type 
  | DOT_DOT_DOT  Type 
  | IdentifierList Type 
  | IdentifierList  DOT_DOT_DOT Type 
  ;


InterfaceType:
  INTERFACE_KEYWORD '{' '}'
  | INTERFACE_KEYWORD '{' RepeatingInterfaceElem '}'
  ;
RepeatingInterfaceElem:
  InterfaceElem ';'
  | RepeatingInterfaceElem InterfaceElem ';'
  ;

InterfaceElem: MethodElem 
  | TypeElem 
  ;
MethodElem: MethodName Signature ;
MethodName: identifier;
TypeElem:  
  TypeTerm
  | TypeElem '|' TypeTerm
  ;
TypeTerm :
  Type 
  | UnderlyingType 
  ;
UnderlyingType : '~' Type ;


MapType: MAP_KEYWORD '[' KeyType ']' ElementType ;
KeyType: Type ;

ChannelType:
  CHAN_KEYWORD  ElementType 
  | CHAN_KEYWORD LEFT_ARROW  ElementType 
  | LEFT_ARROW CHAN_KEYWORD  ElementType 
  ;

Statement :
	Declaration | LabeledStmt | SimpleStmt |
	GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
	FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | // ForStmt |
	DeferStmt ;

SimpleStmt : EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment | ShortVarDecl ;

EmptyStmt : ;
LabeledStmt : Label ':' Statement ;
Label : identifier ;
ExpressionStmt : Expression ;
SendStmt : Channel LEFT_ARROW Expression ;
Channel  : Expression ;
IncDecStmt : Expression  INCREMENT   | Expression  DECREMENT   ;
Assignment : ExpressionList assign_op ExpressionList ;
assign_op : add_op  '='|  mul_op  '=' ;

IfStmt : 
  IF_KEYWORD  Expression Block                                                |
  IF_KEYWORD  Expression Block  ELSE_KEYWORD IfStmtAfterElse                  |
  IF_KEYWORD  SimpleStmt ';'  Expression Block                                |
  IF_KEYWORD SimpleStmt ';'  Expression Block  ELSE_KEYWORD IfStmtAfterElse   ;

IfStmtAfterElse: IfStmt | Block ;

SwitchStmt : ExprSwitchStmt ; //| TypeSwitchStmt ;

ExprSwitchStmt : 
  SWITCH_KEYWORD AfterSwitchKeyWord '{'  '}'  |
  SWITCH_KEYWORD AfterSwitchKeyWord '{' RepeatingExprCaseClause '}'  
  ;

AfterSwitchKeyWord:
  SimpleStmt ';'
  | Expression
  | SimpleStmt ';' Expression
  |
  ;

RepeatingExprCaseClause:
  ExprCaseClause
  | RepeatingExprCaseClause ExprCaseClause
  ;

ExprCaseClause : ExprSwitchCase ':' StatementList ;
ExprSwitchCase : CASE_KEYWORD ExpressionList | DEFAULT_KEYWORD ;
/*
TypeSwitchStmt  : 
  SWITCH_KEYWORD  TypeSwitchGuard '{'  '}' |
  SWITCH_KEYWORD  SimpleStmt ';'  TypeSwitchGuard '{'  '}' |
  SWITCH_KEYWORD  TypeSwitchGuard '{' RepeatingExprCaseClause '}' | ////?????????????????????????
  SWITCH_KEYWORD  SimpleStmt ';'  TypeSwitchGuard '{' RepeatingExprCaseClause '}' | ////?????????????????????????

  ;
TypeSwitchGuard : PrimaryExpr '.' '(' TYPE_KEYWORD ')' |  identifier COLON_EQ  PrimaryExpr '.' '(' TYPE_KEYWORD ')' ;
TypeCaseClause  : TypeSwitchCase ':' StatementList ;
TypeSwitchCase  :  CASE_KEYWORD ExpressionList | DEFAULT_KEYWORD ;

*/

/*
ForStmt : 
  FOR_KEYWORD  Block |
  FOR_KEYWORD FOR_START Block ;
FOR_START: Condition | ForClause | RangeClause ;
Condition : Expression ;

ForClause : 
  ';' ';'                                 |
  ';'';'  PostStmt                        |
  ';' Condition ';'                       |
  ';' Condition ';' PostStmt              |
  InitStmt ';' ';'                        |
  InitStmt ';'  ';'  PostStmt             |
  InitStmt ';'  Condition  ';'            |
  InitStmt ';'  Condition  ';'  PostStmt  ;
InitStmt : SimpleStmt ;
PostStmt : SimpleStmt ;

RangeClause : OptRangeClause RANGE_KEYWORD Expression ;

OptRangeClause:
  ExpressionList '='
  | IdentifierList COLON_EQ 
  |
  ;
*/
GoStmt : GO_KEYWORD Expression ;

SelectStmt : SELECT_KEYWORD '{' '}' | SELECT_KEYWORD '{' RepeatingCommClause '}' ;
RepeatingCommClause: CommClause | RepeatingCommClause CommClause ;
CommClause : CommCase ':' StatementList ;
CommCase   : CASE_KEYWORD SendStmt | CASE_KEYWORD  RecvStmt| DEFAULT_KEYWORD ;
RecvStmt   : ExpressionList '=' RecvExpr| IdentifierList COLON_EQ RecvExpr ;
RecvExpr   : Expression ;

ReturnStmt : RETURN_KEYWORD ExpressionList  | RETURN_KEYWORD ;
BreakStmt  : BREAK_KEYWORD  Label  | BREAK_KEYWORD ;
ContinueStmt: CONTINUE_KEYWORD Label | CONTINUE_KEYWORD ;
GotoStmt:       GOTO_KEYWORD Label      | GOTO_KEYWORD ;
FallthroughStmt : FALL_KEYWORD;
DeferStmt : DEFER_KEYWORD Expression ;



Type:
  TypeName OptTypeArgs
  | TypeLit
  | '(' Type ')'
  ;

TypeName:
  identifier
  | QualifiedIdent
  ;

OptTypeArgs:
  TypeArgs
  |
  ;


TypeArgs:
  '[' TypeList ',' ']'
  | '[' TypeList  ']'
  ;

TypeList:
  Type
  | TypeList ',' Type
  ;


TypeLit:
  ArrayType
  | StructType
  | PointerType 
  | FunctionType 
  | InterfaceType
  | SliceType 
  | MapType
  | ChannelType
  | INT_TYPE 
  | FLOAT_TYPE 
  | COMPLEX_TYPE 
  | BOOL_TYPE 
  | STRING_TYPE
  ;

ArrayType   : '[' ArrayLength ']' ElementType ;
ArrayLength : Expression ;
ElementType : Type ;

SliceType : '['  ']' ElementType ;

StructType    : 
  STRUCT_KEYWORD '{'  '}' 
  | STRUCT_KEYWORD '{' RepeatingFieldDecl '}' 
  ;

RepeatingFieldDecl:
  FieldDecl ';'
  | RepeatingFieldDecl FieldDecl ';'
  ;
FieldDecl :
  IdentifierList Type
  | EmbeddedField
  | IdentifierList Type Tag
  | EmbeddedField Tag
  ;
EmbeddedField : 
  TypeName
  | TypeName TypeArgs
  | '*' TypeName 
  | '*' TypeName TypeArgs
  ;

Tag : string_lit;

PointerType : '*' BaseType ;
BaseType : Type ; 


Block : 
  '{' StatementList '}' 
  ;
StatementList:
  RepeatingStatementList
  |
  ;
RepeatingStatementList :
  Statement ';'
  | RepeatingStatementList Statement ';'
  ;

Operand  : Literal 
  | OperandName 
  | OperandName  TypeArgs  
  | '(' Expression  ')'         { print("Operand ( expression) "); }
  ;
Literal  : BasicLit | CompositeLit | FunctionLit ;
BasicLit : int_lit | float_lit | bool_lit | imaginary_lit |  string_lit  | NIL_KEYWORD;
OperandName : identifier  | QualifiedIdent   ;

QualifiedIdent : PackageName '.' identifier ;

CompositeLit : LiteralType LiteralValue ;
LiteralType   : StructType | ArrayType | '[' DOT_DOT_DOT ']' ElementType |
                SliceType | MapType | TypeName   | TypeName  TypeArgs ;
LiteralValue  : '{' '}' | '{'  ElementList  '}' | '{'  ElementList ','  '}';
ElementList   : KeyedElement | ElementList ',' KeyedElement ;
KeyedElement  :  Element |  Key ':'  Element ;
Key           : FieldName | Expression | LiteralValue ;
FieldName    : identifier ;
Element      : Expression | LiteralValue ;

FunctionLit : FUNC_KEYWORD Signature FunctionBody ;

PrimaryExpr :
	Operand |
	Conversion |
	MethodExpr |
	PrimaryExpr Selector |
	PrimaryExpr Index |
	PrimaryExpr Slice |
	PrimaryExpr TypeAssertion |
	PrimaryExpr Arguments     { print("PrimaryExpression Arguments"); }
  ;

Selector : '.' identifier ;
Index : '[' Expression ']' ;
Slice : '['  ':'  ']' |
        '['  ':'  Expression  ']' |
        '['  Expression  ':'  ']' |
        '['  Expression  ':'  Expression  ']' |
        '['  ':' Expression ':' Expression ']' |
        '['  Expression  ':' Expression ':' Expression ']' 
      ;

TypeAssertion : '.' '(' Type ')' ;

Arguments  : 
  '(' ')'             
  | '('   ExpressionList  ArgumentsAppendix ')'             
  | '('   Type   ArgumentsAppendix ')'
  | '('   Type  ',' ExpressionList   ArgumentsAppendix ')'
  ;


ArgumentsAppendix:
  DOT_DOT_DOT
  | ','
  | DOT_DOT_DOT ','
  |
  ;

MethodExpr: ReceiverType '.' MethodName ;
ReceiverType : Type ;


Expression : UnaryExpr | Expression binary_op Expression ;
UnaryExpr  : PrimaryExpr | unary_op UnaryExpr ;

binary_op  : 
  LOGICAL_OR        %prec PREC_1
  | LOGICAL_AND     %prec PREC_2
  | rel_op          %prec PREC_3   
  | add_op          %prec PREC_4  
  | mul_op          %prec PREC_5  
  ;
rel_op     : EQ_RELATION | GREATER_RELATION | LESS_RELATION | GREATER_EQ_RELATION | LESS_EQ_RELATION | NOT_EQ_RELATION ;
mul_op     :  '/' | '%' | SHIFT | '&';
add_op     : '+' | '-' | '!' | '^' ;
unary_op   : '+' | '-' | '!' | '^' | '*' | '&' | LEFT_ARROW;

Conversion : Type '(' Expression  ')' | Type '(' Expression ',' ')' ;

%%



void yyerror(char * msg) {
fprintf(stderr, "(line %d) %s",  line_number, msg);

exit(1);
}
