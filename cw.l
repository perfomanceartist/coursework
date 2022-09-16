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
%x C_SEP
%%
"/*"            { BEGIN(C_COMMENT); }
<C_COMMENT>"*/" { BEGIN(INITIAL); }
<C_COMMENT>\n   { line_number++; }
<C_COMMENT>.    { }

<C_SEP>\n		{ BEGIN(INITIAL); return ';'; }
<C_SEP>.		{ BEGIN(INITIAL); }

'\n'		{  }

[/][/].*    {  ;					}

break		{ BEGIN(C_SEP); return BREAK_KEYWORD;  	}
case 		{ return CASE_KEYWORD;		}
chan		{ return CHAN_KEYWORD;		}
const 		{ return CONST_KEYWORD; 	}
continue	{ BEGIN(C_SEP); return CONTINUE_KEYWORD;  }
default 	{ return DEFAULT_KEYWORD; 	}
defer 		{ return DEFER_KEYWORD; 	}
else		{ return ELSE_KEYWORD;		}	
fallthrough	{ BEGIN(C_SEP); return FALL_KEYWORD;		}
for			{ return FOR_KEYWORD;  		}
func		{ return FUNC_KEYWORD; 		}
go			{ return GO_KEYWORD; 		}
goto		{ return GOTO_KEYWORD;		}
if			{ return IF_KEYWORD;		}
import 		{ return IMPORT_KEYWORD;  	}
interface 	{ return INTERFACE_KEYWORD; }
map			{ return MAP_KEYWORD;		}
package 	{ return PACKAGE_KEYWORD; 	}
range		{ return RANGE_KEYWORD;  	}
return		{ BEGIN(C_SEP); return RETURN_KEYWORD; 	}
select		{ return SELECT_KEYWORD;	}
struct 		{ return STRUCT_KEYWORD; 	}	
switch 		{ return SWITCH_KEYWORD; 	}
type		{ return TYPE_KEYWORD; 		}	
var 		{ return VAR_KEYWORD;  		}

nil			{ return NIL_KEYWORD; 		}


[-]?[0-9]+ 			{ BEGIN(C_SEP); return int_lit;	   		}
[-]?0(xX)[0-9]+ 	{ BEGIN(C_SEP); return int_lit;	   		}
[-]?0(bB)[0-9]+ 	{ BEGIN(C_SEP); return int_lit;	   		}

[-]?[0-9]+[\.][0-9]+ 		{ BEGIN(C_SEP); return float_lit;	}
[-]?0(xX)[0-9]+[\.][0-9]+ 	{ BEGIN(C_SEP); return float_lit;	}
[-]?0(bB)[0-9]+[\.][0-9]+ 	{ BEGIN(C_SEP); return float_lit;	}

true|false 					{ return bool_lit;   	}
[-]?[0-9]+[[\+][0-9]+[i]]? 	{ BEGIN(C_SEP); return imaginary_lit; }
\"(\\.|[^"\\])*\"	{ BEGIN(C_SEP); return string_lit; }
\`(\\.|[^"\\])*\`	{ BEGIN(C_SEP); return string_lit; }


:=			{ return COLON_EQ; 	}





bool		{ return BOOL_TYPE; 		}
string 		{ return STRING_TYPE; 		}
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

[A-Za-z_]+[A-Za-z_0-9]* { BEGIN(C_SEP); return identifier;		}

[+\-*\/%&\|\^=\~!]		{ return yytext[0]; }

>>|<<	{ return SHIFT;		}

&&		{return LOGICAL_AND; }
"||"		{ return LOGICAL_OR;	}
"<-"			{ return LEFT_ARROW; 	}
"++"			{ BEGIN(C_SEP); return INCREMENT;		}
"--"			{ BEGIN(C_SEP); return DECREMENT; 	}

"=="			{ return EQ_RELATION;	}
"<="			{ return LESS_EQ_RELATION;}
">="			{ return GREATER_EQ_RELATION;}
"<"			{ return LESS_RELATION; }
">" 			{ return GREATER_RELATION; }
"..."			{ return DOT_DOT_DOT;	}

[(\.,\{\:\[] { return yytext[0]; }
\)				 { BEGIN(C_SEP); return yytext[0];}
\]				 { BEGIN(C_SEP); return yytext[0];}
\}				 { BEGIN(C_SEP); return yytext[0];}


%%
