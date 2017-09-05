default: benchmark-results.txt
.PHONY: default

GRAMMAR = ./corp/create_lim_grammar.py
NAME ?= corp/pmcfg-5.readable
PTK ?= 10


${NAME}.classes:
	${GRAMMAR} ${NAME}

${NAME}.gr:
	${GRAMMAR} ${NAME}

benchmark-results.txt: ${NAME}.gr ${NAME}.classes ${NAME}.txt
	cargo run coarse-to-fine benchmark ${NAME}.gr ${NAME}.classes ${NAME}.txt ${PTK}

.PHONY: clean clean-all
clean:
	rm -fv ${NAME}.gr ${NAME}.classes benchmark-results.txt

clean-all: clean
