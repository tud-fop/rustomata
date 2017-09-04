default: benchmark-results.txt
.PHONY: default

GRAMMAR = ./corp create_lim_grammar.py
NAME ?= corp/pmcfg-50.readable
PTK ?= 10


LOGS = logs
TIME := $(shell date +"%y%m%d-%H%M")

%.classes:
	${GRAMMAR} %

%.gr:
	${GRAMMAR} %

benchmark-results.txt:
	cargo run coarse-to-fine benchmark ${NAME}.gr ${NAME}.classes ${NAME}.txt ${PTK}

.PHONY: clean clean-all
clean:
	rm -fv ${NAME}.gr ${NAME}.classes benchmark-results.txt

clean-all: clean
