default: rustomata
.PHONY: default

GRAMMAR = ./corp/create_lim_grammar.py
NAME ?= corp/pmcfg-5
PTK ?= 5
EQC ?= corp/pmcfg-5
WORDS ?= 1
NFA ?= false


${EQC}.classes:
	${GRAMMAR} ${EQC}

${NAME}.gr:
	${GRAMMAR} ${NAME}

bench: benchmark-results.txt

benchmark-results.txt: ${NAME}.gr ${NAME}.classes ${NAME}.txt
	target/debug/rustomata coarse-to-fine benchmark ${NAME}.gr ${EQC}.classes ${NAME}.txt ${PTK} --wordlimit ${WORDS} --nfabool ${NFA} 2> benchmark.log

rustomata: target/debug/rustomata

target/debug/rustomata:
	cargo build

.PHONY: clean clean-all
clean:
	rm -fv ${NAME}.gr ${NAME}.classes benchmark-results.txt

clean-all: clean
	rm -fv target/debug/rustomata
