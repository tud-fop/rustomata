default: rustomata
.PHONY: default

GRAMMAR ?= example/example_mcfg.gr
PTK ?= 5
EQC ?= example/example_equivalence.classes
WORDS ?= example/example_word.txt
WORDLIMIT ?=
NFA ?= false

bench: benchmark-results.txt

benchmark-results.txt: ${NAME}.gr ${NAME}.classes ${NAME}.txt
	target/debug/rustomata coarse-to-fine benchmark ${GRAMMAR} ${EQC} ${WORDS} ${PTK} --wordlimit ${WORDLIMIT} --nfabool ${NFA} 2> benchmark.log

rustomata: target/debug/rustomata

target/debug/rustomata:
	cargo build

.PHONY: clean clean-all
clean:
	rm -fv benchmark-results.txt benchmark.log

clean-all: clean
	rm -fv target/debug/rustomata
