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
[`].*		{  ;					}




package 	{ ins_token = 0; return PACKAGE_KEYWORD; }
import 		{ ins_token = 0; return IMPORT_KEYWORD;  }
func		{ ins_token = 0; return FUNC_KEYWORD; 	}
if			{ ins_token = 0; return IF_KEYWORD;	}
else		{ ins_token = 0; return ELSE_KEYWORD;	}
switch 		{ ins_token = 0; return SWITCH_KEYWORD; }
case 		{ ins_token = 0; return CASE_KEYWORD;	}
default 	{ ins_token = 0; return DEFAULT_KEYWORD; }

for			{ ins_token = 0; return FOR_KEYWORD;  	}
break		{ ins_token = 1; return BREAK_KEYWORD;  	}
continue	{ ins_token = 1; return CONTINUE_KEYWORD;   }
range		{ ins_token = 0; return RANGE_KEYWORD;  					}
return		{ ins_token = 1; return RETURN_KEYWORD; 	}
falltrough	{ ins_token = 1; return FALL_KEYWORD;		}
var 		{ ins_token = 0; return VAR_KEYWORD;  }


select		{ ins_token = 0; return SELECT_KEYWORD; }
const 		{ ins_token = 0; return CONST_KEYWORD; 	}
type		{ ins_token = 0; return TYPE_KEYWORD; }	
struct 		{ ins_token = 0; return STRUCT_KEYWORD; }
interface 	{ ins_token = 0; return INTERFACE_KEYWORD; }
chan		{ ins_token = 0; return CHAN_KEYWORD;   }
defer 		{ ins_token = 0; return DEFER_KEYWORD; }
goto		{ ins_token = 0; return GOTO_KEYWORD;	}	
go			{ ins_token = 0; return GO_KEYWORD; 	}
map 		{ ins_token = 0; return MAP_KEYWORD;	}

fallthrough	{ ins_token = 0; return FALL_KEYWORD; 	}

">>"		{ ins_token = 0; return SHIFT_RIGHT; 	}
"<<"		{ ins_token = 0; return SHIFT_LEFT;	}
[-]?[0-9]+ 					{ ins_token = 1; return INTEGER;	    }
[-]?[0-9]+[\.][0-9]+ 		{ ins_token = 1; return FLOAT;	 		}
true 						{ ins_token = 1; return TRUE_FALSE;  	}	
false						{ ins_token = 1; return TRUE_FALSE;   	}
[-]?[0-9]+[[\+][0-9]+[i]]? 	{ ins_token = 1;return COMPLEX; 		}		
\"(\\.|[^"\\])*\"			{ ins_token = 1;return STRING; 			}

[A-Za-z_]+[A-Za-z_0-9]* 	{ ins_token = 1; return IDENTIFICATOR;  }

:= 							{ ins_token = 0; return COLON_EQ;		}

"=="		{ ins_token = 0; return EQ_RELATION; }
">"			{ ins_token = 0; return GREATER_RELATION; }
"<" 		{ ins_token = 0; return LESS_RELATION; }
">=" 		{ ins_token = 0; return EQ_GREATER_RELATION; }
"<="		{ ins_token = 0; return EQ_LESS_RELATION; }
"!="		{ ins_token = 0; return NOT_EQ_RELATION; }



"++"		{ ins_token = 1; return INCREMENT; 	}	
"--"		{ ins_token = 1; return DECREMENT; 	}	

"!"			{ ins_token = 0; return NOT_OPERATION; }
"&&"		{ ins_token = 0; return AND_OPERATION; }
"||"		{ ins_token = 0; return OR_OPERATION; }

"..."		{ ins_token = 0; return DOT_DOT_DOT; }
"<-"		{ ins_token = 0; return LEFT_ARROW; }
[-+=] 		{ ins_token = 0; return yytext[0];		}
[\*]*       		 	{	ins_token = 0; return '*'; }

[\(\.,\{\:\[] 			{   ins_token = 0;  return yytext[0]; 	}
\)				 		{   ins_token = 1; return yytext[0];	}
\]				 		{   ins_token = 1; return yytext[0];	}
\}				 		{   ins_token = 1; return yytext[0];	}



[ \t\f\v] 				{ ; 					}
\n						{ 
							int flag = ins_token;
							ins_token = 0;
							line_number++; 
							//printf("Lex: newline(token=%d)\n", ins_token);
							if (flag == 1) return ';'; 
						}
;						{ ins_token = 0;  return ';'; }



"&"			{  return yytext[0];	}
"|" 		{  return yytext[0];	}

"^" 		{  return yytext[0];	}


%%