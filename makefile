.PHONY: lint all exec
SHELL = /usr/bin/bash

all:
	cabal build

exec:
	cabal run chatnega

lint:
	for file in $$(find src -name *.hs); do\
		ormolu -minplace $$file;\
	done
