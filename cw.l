%{

#include <stdio.h>
#include <stdlib.h>
#include "cw.tab.h"

	unsigned line_number;
    extern int yylex();
	extern void yyerror(char * msg);
	FILE *inputStream;

	#define yyin inputStream
	#define tmp_scanf(f_, ...) fscanf(yyin, (f_), __VA_ARGS__)

	int yywrap()
	{
			return 1;
	} 
	void print(char* msg) {
		printf("(line %d). %s\n", line_number, msg);
	}

	int main(int argc, char* argv[])
	{
		line_number = 1;

		inputStream = fopen(argv[1], "r");
		
		if (inputStream == NULL)
		{
			printf("File wasn't found\n");
			return -1;
		}

		yyparse();

		fclose(inputStream);
	}
%}


%x C_COMMENT

%%
"/*"            { BEGIN(C_COMMENT); }
<C_COMMENT>"*/" { BEGIN(INITIAL); }
<C_COMMENT>\n   { line_number++; }
<C_COMMENT>.    { }

[/][/].*    {  ;					}



package 	{ return PACKAGE_KEYWORD; }
import 		{ return IMPORT_KEYWORD;  }
func		{ return FUNC_KEYWORD; 	}
if			{ return IF_KEYWORD;	}
else		{ return ELSE_KEYWORD;	}
switch 		{  return SWITCH_KEYWORD; }
case 		{ return CASE_KEYWORD;	}
default 	{ return DEFAULT_KEYWORD; }

for			{ return FOR_KEYWORD;  	}
break		{ return BREAK_KEYWORD;  	}
continue	{ return CONTINUE_KEYWORD;  }
range		{ return RANGE_KEYWORD;  	}
return		{ return RETURN_KEYWORD; 	}

var 		{ return VAR_KEYWORD;  }
int8		{ return INT_TYPE; }
int16		{ return INT_TYPE; }
int32 		{ return INT_TYPE; }
int64		{ return INT_TYPE; }
uint8		{ return INT_TYPE; }
uint16 		{ return INT_TYPE; }
uint32		{ return INT_TYPE; }
uint64		{ return INT_TYPE; }
byte		{ return INT_TYPE; }
rune		{ return INT_TYPE; }
int			{ return INT_TYPE; }
uint		{ return INT_TYPE; }

float32		{ return FLOAT_TYPE; }
float64		{ return FLOAT_TYPE; }

complex64	{ return COMPLEX_TYPE; }
complex128	{ return COMPLEX_TYPE; }

bool		{ return BOOL_TYPE; 	}
string 		{ return STRING_TYPE; 	}

const 		{ return CONST_KEYWORD; 	}
type		{ return TYPE_KEYWORD; }	
struct 		{ return STRUCT_KEYWORD; }
interface 	{ return INTERFACE_KEYWORD; }

nil			{ return NIL_KEYWORD; 	}
defer 		{ return DEFER_KEYWORD; }

"go"		{ return GO_KEYWORD; }

">>"		{ return SHIFT_RIGHT; 	}
"<<"		{ return SHIFT_LEFT;	}
[-]?[0-9]+ 	{ return INTEGER;	   }
[-]?[0-9]+[\.][0-9]+ 	{ return FLOAT;	 		    }
true 		{ return TRUE_FALSE;   }
false		{ return TRUE_FALSE;   }
[-]?[0-9]+[[\+][0-9]+[i]]? { return COMPLEX; }
\"(\\.|[^"\\])*\"	{ return STRING; }
:= return COLON_EQ;
[A-Za-z_]+[A-Za-z_0-9]* { return IDENTIFICATOR;		}

"=="			{ return EQ_RELATION; }
">"			{ return GREATER_RELATION; }
"<" 			{ return LESS_RELATION; }
">=" 		{ return EQ_GREATER_RELATION; }
"<="		{ return EQ_LESS_RELATION; }
"!="		{ return NOT_EQ_RELATION; }

"+="		{ return OPER_ASSIGNMENT;	}
"-="		{ return OPER_ASSIGNMENT;	}
"*="		{ return OPER_ASSIGNMENT;	}
"/="		{ return OPER_ASSIGNMENT;	}
"%="		{ return OPER_ASSIGNMENT;	}
">>="		{ return OPER_ASSIGNMENT;	}
"<<="		{ return OPER_ASSIGNMENT;	}
"|="		{ return OPER_ASSIGNMENT;	}
"^="		{ return OPER_ASSIGNMENT;	}


"++"		{ return INCREMENT; 	}	
"--"		{ return DECREMENT; 	}	

"!"			{ return NOT_OPERATION; }
"&&"		{ return AND_OPERATION; }
"||"		{ return OR_OPERATION; }

"..."		{ return DOT_DOT_DOT; }

[-+()=\.,\{\}\:\[\]] { return yytext[0];		}
[\*]*       		 {return '*'; }
[ \t\f\v] 	{ ; 					}
"\n"		{  line_number++; return yytext[0];	}
"&"			{  return yytext[0];	}
"|" 		{  return yytext[0];	}

"^" 		{  return yytext[0];	}
";"         {  return yytext[0];	}


%%
