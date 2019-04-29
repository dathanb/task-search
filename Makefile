.PHONY: build clean

build:
	stack build

clean:
	rm -rf out/ .stack-work/

