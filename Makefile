Y = TypeParse.y
YH = TypeParse.hs
SRC = TypeInf.hs

all : TypeInf.o TypeParse.o Eval.o

%.o : %.hs
	ghc -Wall -O2 -c $*.hs

$(YH) : $(Y)
	happy $(Y)

clean :
	rm -rf *.o *.hi $(YH)
