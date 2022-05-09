all:	executable

executable: y.tab.c lex.yy.c
	gcc  cw.tab.c lex.yy.c  -o recogniser.exe
y.tab.c: cw.y
	./bison/win_bison.exe -d  cw.y 

lex.yy.c: cw.l
	./bison/win_flex cw.l


clean:
	rm recogniser.exe y.tab.c lex.yy.c


	
	
	
	

