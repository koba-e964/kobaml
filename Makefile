Y = TypeParse.y ExprParser.y ExprLexer.x
YH = TypeParse.hs ExprParser.hs ExprLexer.hs

all : TypeInf.o TypeParse.o Eval.o ExprToken.o ExprParser.o ExprLexer.o

%.o : %.hs
	ghc -Wall -O2 -c $*.hs

%.hs : %.y
	happy $*.y

%.o : %.x
	alex $*.x
	ghc -O2 -c $*.hs
clean :
	rm -rf *.o *.hi $(YH)
