GHC = ghc
HLINT = hlint

SRC_FILES = src/*.hs

all: bin/expose

bin/%: src/%.hs
	$(GHC) --make -Wall -outputdir bin -o $@ $<

hlint:
	$(HLINT) $(wildcard $(SRC_FILES))

.PHONY: all hlint
