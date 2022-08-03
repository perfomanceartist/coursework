%{

#include <stdio.h>
#include <stdlib.h>
#include "cw.tab.h"

	extern unsigned line_number;
    extern int yylex();
	extern void yyerror(char * msg);
	FILE *inputStream;

	#define yyin inputStream
	#define tmp_scanf(f_, ...) fscanf(yyin, (f_), __VA_ARGS__)

	int yywrap()
	{
			return 1;
	} 

	int main(int argc, char* argv[])
	{
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




%%
[/][/].*    {  ;					}



package 	{ return PACKAGE_KEYWORD; }
import 		{ return IMPORT_KEYWORD;  }
func		{ return FUNC_KEYWORD; 	}
if			{ return IF_KEYWORD;	}
else		{ return ELSE_KEYWORD;	}
switch 		{  return SWITCH_KEYWORD; }
case 		{ return CASE_KEYWORD;	}
default 	{ return DEFAULT_KEYWORD; }
var 		{ return VAR_KEYWORD;	}
int8		{ return TYPE_KEYWORD; }
int16		{ return TYPE_KEYWORD; }
int32 		{ return TYPE_KEYWORD; }
int64		{ return TYPE_KEYWORD; }
uint8		{ return TYPE_KEYWORD; }
uint16 		{ return TYPE_KEYWORD; }
uint32		{ return TYPE_KEYWORD; }
uint64		{ return TYPE_KEYWORD; }
byte		{ return TYPE_KEYWORD; }
rune		{ return TYPE_KEYWORD; }
int			{ return TYPE_KEYWORD; }
uint		{ return TYPE_KEYWORD; }

float32		{ return TYPE_KEYWORD; }
float64		{ return TYPE_KEYWORD; }

complex64	{ return TYPE_KEYWORD; }
complex128	{ return TYPE_KEYWORD; }

bool		{ return TYPE_KEYWORD; }
string 		{ return TYPE_KEYWORD; }

const 		{ return CONST_KEYWORD;}
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

"!"			{ return NOT_OPERATION; }
"&&"		{ return AND_OPERATION; }
"||"		{ return OR_OPERATION; }

"..."		{ return DOT_DOT_DOT; }

[-+()\*=\.,\{\}\:] { return yytext[0];		}
[ \t\f\v] 	{ ; 					}
"\n"		{  return yytext[0];	}



"^" 		{ return '^'; 			}
";"         { return ';'; 			}


%%
