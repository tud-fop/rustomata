default: rustomata
.PHONY: default

GRAMMAR = ./corp/create_lim_grammar.py
NAME ?= corp/pmcfg-5
PTK ?= 5


${NAME}.classes:
	${GRAMMAR} ${NAME}

${NAME}.gr:
	${GRAMMAR} ${NAME}

benchmark: benchmark-results.txt

benchmark-results.txt: ${NAME}.gr ${NAME}.classes ${NAME}.txt
	target/debug/rustomata coarse-to-fine benchmark ${NAME}.gr ${NAME}.classes ${NAME}.txt ${PTK}

rustomata: target/debug/rustomata

target/debug/rustomata:
	cargo build

.PHONY: clean clean-all
clean:
	rm -fv ${NAME}.gr ${NAME}.classes benchmark-results.txt

clean-all: clean
	rm -fv target/debug/rustomata
