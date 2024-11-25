HC = ghc
HC_OPTS = --make -outputdir ./obj

ALEX = alex
ALEX_OPTS = -g

HAPPY = happy
HAPPY_OPTS=-agc

SRC = src

ifeq ($(OS),Windows_NT)
	PROG = interpreterWIN64
else

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S), Linux)
	PROG = hasktan
endif
ifeq ($(UNAME_S), Darwin)
	PROG = interpreterOSX
endif
endif

TESTS := $(wildcard tests/*.hs)



%.alex.hs : $(SRC)/%.x
	$(ALEX) $(ALEX_OPTS) $< -o $@

%.happy.hs : $(SRC)/%.y
	$(HAPPY) $(HAPPY_OPTS) $< -o $@

$(PROG) : Lexer.alex.hs Grammar.happy.hs $(SRC)/TypeChecker.hs $(SRC)/Interpreter.hs
	$(HC) $(HC_OPTS) -o $@ $^


all: 
	@if [ -f $(PROG) ] && [ "$$(find . -name $(PROG) -newer $(SRC)/Interpreter.hs)" ]; then \
		$(HAPPY) $(SRC)/Grammar.y -i; \
	else \
		$(MAKE) $(PROG); \
	fi


.PHONY: clean test

clean:
	rm -rf obj *.o *.hi $(SRC)/*.info $(SRC)/Grammar.hs \
		*.alex.hs *.happy.hs $(PROG)

test: all
	@for file in $(TESTS); do \
		echo "\033[1;33;41m"; \
		cat "$$file"; \
		echo "\033[0m"; \
		./$(PROG) $$file; \
		echo ""; \
	done
