Y = TypeParse.y ExprParser.y ExprLexer.x
YH = TypeParse.hs ExprParser.hs ExprLexer.hs
OBJS = TypeInf.o TypeParse.o Eval.o ExprToken.o ExprParser.o ExprLexer.o Main.o
EXEC = mcalc

$(EXEC) : $(OBJS)
	ghc -o $@ Main.hs

%.o : %.hs
	ghc -Wall -O2 -c $*.hs

%.o : %.y
	happy $*.y
	ghc -O2 -c $*.hs

%.o : %.x
	alex $*.x
	ghc -O2 -c $*.hs
clean :
	rm -rf *.o *.hi $(YH) $(EXEC)
