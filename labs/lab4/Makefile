HC = ghc
HC_OPTS = --make -outputdir ./obj
ALEX = alex
ALEX_OPTS = -g
HAPPY = happy
HAPPY_OPTS=-agc

SRC = src

PROG = lex

%.alex.hs : $(SRC)/%.x
	$(ALEX) $(ALEX_OPTS) $< -o $@

%.happy.hs : %.y
	$(HAPPY) $(HAPPY_OPTS) $< -o $@

%.o : %.hs
	$(HC) $(HC_OPTS) -c -o $@ $<


all : $(PROG)

lex$(exeext) : Lexer.alex.hs
	$(HC) $(HC_OPTS) -o $@ $^

.PHONY: clean
clean:
	rm -rf obj *.o *.hi lex \
		*.alex.hs *.happy.hs
test:
	@./$(PROG) < test.hs
