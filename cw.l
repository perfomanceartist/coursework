%{

#include <stdio.h>
#include <stdlib.h>
#include "cw.tab.h"

	unsigned line_number;
	int ins_token;
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
		ins_token = 0;

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

break		{ ins_token = 1; return BREAK_KEYWORD;  	}
case 		{ ins_token = 0; return CASE_KEYWORD;		}
chan		{ ins_token = 0; return CHAN_KEYWORD;		}
const 		{ ins_token = 0; return CONST_KEYWORD; 	}
continue	{ ins_token = 1; return CONTINUE_KEYWORD;  }
default 	{ ins_token = 0; return DEFAULT_KEYWORD; 	}
defer 		{ ins_token = 0; return DEFER_KEYWORD; 	}
else		{ ins_token = 0; return ELSE_KEYWORD;		}	
fallthrough	{ ins_token = 1; return FALL_KEYWORD;		}
for			{ ins_token = 0; return FOR_KEYWORD;  		}
func		{ ins_token = 0; return FUNC_KEYWORD; 		}
go			{ ins_token = 0; return GO_KEYWORD; 		}
goto		{ ins_token = 0;  return GOTO_KEYWORD;		}
if			{ ins_token = 0; return IF_KEYWORD;			}
import 		{ ins_token = 0; return IMPORT_KEYWORD;  	}
interface 	{ ins_token = 0; return INTERFACE_KEYWORD; 	}
map			{ ins_token = 0; return MAP_KEYWORD;		}
package 	{ ins_token = 0; return PACKAGE_KEYWORD; 	}
range		{ ins_token = 0;  return RANGE_KEYWORD;  	}
return		{ ins_token = 1; return RETURN_KEYWORD; 	}
select		{ ins_token = 0; return SELECT_KEYWORD;		}
struct 		{ ins_token = 0; return STRUCT_KEYWORD; 	}	
switch 		{ ins_token = 0; return SWITCH_KEYWORD; 	}
type		{ ins_token = 0; return TYPE_KEYWORD; 		}	
var 		{ ins_token = 0; return VAR_KEYWORD;  		}

nil			{ ins_token = 0; return NIL_KEYWORD; 		}


[-]?[0-9]+ 			{   ins_token = 1; return int_lit;	   		}
[-]?0(xX)[0-9]+ 	{   ins_token = 1; return int_lit;	   		}
[-]?0(bB)[0-9]+ 	{   ins_token = 1; return int_lit;	   		}

[-]?[0-9]+[\.][0-9]+ 		{   ins_token = 1; return float_lit;	}
[-]?0(xX)[0-9]+[\.][0-9]+ 	{   ins_token = 1; return float_lit;	}
[-]?0(bB)[0-9]+[\.][0-9]+ 	{   ins_token = 1; return float_lit;	}

true|false 					{ ins_token = 1; return bool_lit;   	}
[-]?[0-9]+[[\+][0-9]+[i]]? 	{  ins_token = 1;  return imaginary_lit; }
\"(\\.|[^"\\])*\"	{   ins_token = 1; return string_lit; }
\`(\\.|[^"\\])*\`	{   ins_token = 1; return string_lit; }


:=			{ ins_token = 0; return COLON_EQ; 	}





bool		{ ins_token = 0; return BOOL_TYPE; 		}
string 		{ ins_token = 0; return STRING_TYPE; 		}
int8|int16|int32|int64|uint8|uint16|uint32|uint64|byte|rune|int|uint		{ ins_token = 0;  return INT_TYPE; }
float32|float64																{ ins_token = 0;  return FLOAT_TYPE; }
complex64|complex128														{ ins_token = 0;  return COMPLEX_TYPE; }

[A-Za-z_]+[A-Za-z_0-9]* {   ins_token = 1;  return identifier;		}
[ \t\f\v] 				{ ; 					}
\n						{ 
	int flag = ins_token;
	ins_token = 0;
	line_number++; 
	//printf("Lex: newline(token=%d)\n", ins_token);
	if (flag == 1) return ';'; 
	}
;						{ ins_token = 0;  return ';'; }
[+\-*\/%&\|\^=\~!]		{ ins_token = 0;  return yytext[0]; }

>>|<<			{ ins_token = 0;  return SHIFT;		}

&&				{ ins_token = 0;  return LOGICAL_AND; }
"||"			{ ins_token = 0;  return LOGICAL_OR;	}
"<-"			{ ins_token = 0;  return LEFT_ARROW; 	}
"++"			{ ins_token = 1; return INCREMENT;		}
"--"			{ ins_token = 1; return DECREMENT; 	}

"=="			{ ins_token = 0;  return EQ_RELATION;	}
"<="			{ ins_token = 0;  return LESS_EQ_RELATION;}
">="			{ ins_token = 0;  return GREATER_EQ_RELATION;}
"<"				{ ins_token = 0;  return LESS_RELATION; }
">" 			{ ins_token = 0;  return GREATER_RELATION; }
"..."			{ ins_token = 0;  return DOT_DOT_DOT;	}

[\(\.,\{\:\[] 			{   ins_token = 0;  return yytext[0]; 	}
\)				 		{   ins_token = 1; return yytext[0];	}
\]				 		{   ins_token = 1; return yytext[0];	}
\}				 		{   ins_token = 1; return yytext[0];	}


%%
