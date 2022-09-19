all:	executable

executable: y.tab.c lex.yy.c
	gcc  cw.tab.c lex.yy.c  -o recogniser.exe
y.tab.c: cw.y
	bison -d  cw.y 

lex.yy.c: cw.l
	flex cw.l


clean:
	del recogniser.exe cw.tab.c lex.yy.c


	
	
	
	

