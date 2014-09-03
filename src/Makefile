Y = ExprParser.y ExprLexer.x
YH = ExprParser.hsy ExprLexer.hsx
YHS = ExprParser.hs ExprLexer.hs
OBJS = CDef.o ExprToken.o ExprLexer.o ExprParser.o Eval.o TypeInf.o Main.o
EXEC = mcalc

$(EXEC) : *.hs $(YHS)
	ghc -Wall -O2 -o $@ Main.hs

%.o : %.hs
	ghc -Wall -O2 -c $*.hs

%.hs : %.y
	happy $*.y -o $*.hs
	cp $*.hs $*.hsy
%.hs : %.x
	alex $*.x -o $*.hs
	cp $*.hs $*.hsx
clean :
	rm -rf *.o *.hi $(YH) $(YHS) $(EXEC)
