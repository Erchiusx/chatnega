.PHONY: lint all exec
SHELL = /usr/bin/bash

all:
	cabal build

exec:
	cabal run $$(basename $$(dir *.cabal) .cabal)

lint:
	fourmolu -i src
