Y = TypeParse.y ExprParser.y ExprLexer.x
YH = TypeParse.hsy ExprParser.hsy ExprLexer.hsx
OBJS = Eval.o ExprToken.o ExprLexer.o ExprParser.o TypeInf.o TypeParse.o Main.o
EXEC = mcalc

$(EXEC) : $(OBJS)
	ghc -o $@ Main.hs

%.o : %.hs
	ghc -Wall -O2 -c $*.hs

%.hs : %.y
	happy $*.y -o $*.hs
	cp $*.hs $*.hsy
%.hs : %.x
	alex $*.x -o $*.hs
	cp $*.hs $*.hsx
clean :
	rm -rf *.o *.hi $(YH) $(EXEC)
