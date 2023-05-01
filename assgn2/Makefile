CXX   = g++-8
EXE = iplC
CXXDEBUG = -g -Wall
CXXSTD = -std=c++11


.PHONY: all
all: parser lexer 	
	$(CXX) $(CXXDEBUG) ${CXXSTD} -o ${EXE} driver.cpp parser.o scanner.o 

parser: parser.yy scanner.hh ast.hh printast.hh
	bison -d -v -Wmidrule-value $<
	$(CXX) $(CXXDEBUG) ${CXXSTD} -c parser.tab.cc -o parser.o 

lexer: scanner.l scanner.hh parser.tab.hh parser.tab.cc
	flex++ --outfile=scanner.yy.cc  $<
	$(CXX)  $(CXXDEBUG) ${CXXSTD} -c scanner.yy.cc -o scanner.o

clean:
	rm *.o *.cc parser.tab.hh stack.hh location.hh position.hh parser.output ${EXE}