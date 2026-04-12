.PHONY: test lint byte-compile clean

test:
	eask test ert tests/anvil-test.el

lint:
	eask lint package
	eask lint checkdoc

byte-compile:
	eask compile

clean:
	eask clean all
