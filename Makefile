Y = TypeParse.y ExprParser.y
YH = TypeParse.hs ExprParser.hs

all : TypeInf.o TypeParse.o Eval.o ExprParser.hs

%.o : %.hs
	ghc -Wall -O2 -c $*.hs

%.hs : %.y
	happy $*.y

clean :
	rm -rf *.o *.hi $(YH)
