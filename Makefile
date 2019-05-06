.PHONY: build test clean

build:
	stack build

test: build
	stack test

clean:
	rm -rf out/ .stack-work/

