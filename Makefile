all:	executable

executable: y.tab.c lex.yy.c
	gcc  cw.tab.c lex.yy.c  -o recogniser.exe
y.tab.c: cw.y
	./bison/win_bison.exe   -d  cw.y  --warnings=counterexamples

lex.yy.c: cw.l
	./bison/win_flex cw.l


clean:
	del recogniser.exe cw.tab.c lex.yy.c


	
	
	
	

