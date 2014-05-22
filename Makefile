Y = TypeParse.y
YH = TypeParse.hs
SRC = TypeInf.hs

all : TypeInf.o TypeParse.o

%.o : %.hs
	ghc -Wall -O2 $*

$(YH) : $(Y)
	happy $(Y)
