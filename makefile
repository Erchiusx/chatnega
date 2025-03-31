.PHONY: lint
SHELL = /usr/bin/bash

lint:
	for file in $$(find src -name *.hs); do\
		ormolu -minplace $$file;\
	done
