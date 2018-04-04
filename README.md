# rustomata

Framework for (weighted) automata with storage


## example usage

* create a grammar file
```bash
cat <<EOF > example.mcfg
initial: [S]
S → [[Var 0 0, Var 1 0, Var 0 1, Var 1 1]] (A, B)
A → [[T a, Var 0 0],  [T c, Var 0 1]     ] (A   )   # 0.5
A → [[],  []                             ] (    )   # 0.5
B → [[T b, Var 0 0],  [T d, Var 0 1]     ] (B   )   # 0.5
B → [[],  []                             ] (    )   # 0.5
EOF
```

* construct a tree-stack automaton from the given grammar, print out that automaton
```bash
cargo run mcfg automaton grammar.gr
```

* construct a tree-stack automaton from the given grammar and recognise the word "a a b c c d", print out a best (maximum weight) accepting configuration (if there is one)
```bash
echo "a a b c c d" | cargo run mcfg parse grammar.gr
```


## the grammar formats

Rustomata can deal with multiple context-free grammars (short: MCFGs) and context-free grammars (short: CFGs).
MCFGs are expressively equivalent to linear context-free rewriting systems (LCFRSs) and simple range concatenation grammars (sRCG).
MCFGs are internally represented by tree-stack automata (short: TSA) and CFG are represented by pushdown automata (short: PDA).
The weights are (for now) assumed to be from the algebra (ℝ₊, ⋅, 1) of non-negative reals with multiplication.

* an example of an MCFG (in the notation of an sRCG):
```
initial non-terminals: only S

productions:
S(x₁y₁x₂y₂) ← A(x₁, x₂) B(y₁, y₂)  with weight 1
A(ax₁, cx₂) ← A(x₁, x₂)            with weight 0.5
A(ε, ε)     ← ε                    with weight 0.5
B(by₁, dy₂) ← B(y₁, y₂)            with weight 0.5
B(ε, ε)     ← ε                    with weight 0.5
```

* the same grammar in rustomata's notation:
```
initial: [S]

S → [[Var 0 0, Var 1 0, Var 0 1, Var 1 1]] (A, B)
A → [[T a, Var 0 0],  [T c, Var 0 1]     ] (A   )   # 0.5
A → [[],  []                             ] (    )   # 0.5
B → [[T b, Var 0 0],  [T d, Var 0 1]     ] (B   )   # 0.5
B → [[],  []                             ] (    )   # 0.5
```

* Lines that contain no non-whitespace characters are ignored by the parser.

* Lines whose first non-whitespace character is a `%` are also ignored (comments).

* A single production *may not* contain newline characters.

* A production may be followed by a comment (starting with `%`).

* The weight definition (e.g. `# 1.0`) may be omitted.  Rustomata then assumes a weight of `1.0`.

* an example of a CFG:
```
initial non-terminals: S

S → a S b    with weight 0.4
S → ε        with weight 0.6

```

* the same grammar in rustomata's notation:
```
initial: [S]

S → [T a, Nt A, Tb]  # 0.4
S → []               # 0.6
```

* The parser specifics of MCFGs also apply for CFGs.


## constructing automata

* create a tree-stack automaton that is equivalent to the given MCFG:
```bash
cargo run mcfg automaton example.mcfg
```

* create a push-down automaton that is equivalent to the given CFG:
```bash
cargo run cfg automaton example.cfg
```


## recognition functionality

* parse an MCFG (internally constructing an tree-stack automaton):
```bash
cargo run mcfg parse grammar.gr
```

* parse a CFG (internally constructing a pushdown automaton):
```bash
cargo run cfg parse grammar.gr
```

## Chomsky-Schützenberger parsing for LCFRS

* extract an internal Chomsky-Schützenberger representation as binary file
```bash
cat "examples/example.pmcfg" | cargo run -- csparsing extract > example.cs
```

* parse a space separated word using a C-S representation file
```bash
echo "a a b c c d" | cargo run -- csparsing parse example.cs
```

## approximation

Rustomata contains several approximation strategies, allowing the transformation of automata with storage into other automata with storage. Available are

* approximation of an MCFG (via a tree-stack automaton) by a pushdown automaton:
```bash
cargo run approximation tts automaton example.mcfg
```

* parse a word with the approximation automaton for the given MCFG:
```bash
cargo run approximation tts parse example.mcfg
```

* approximate a CFG (via a pushdown automaton) by a pushdown automaton using an equivalence relation on the non-terminal symbols.  An equivalence relation is defined by specifying equivalence classes:
```
S [S    ]
N [A, B ]
R *
```
  with `*` matching the remaining non-terminals.
  To get the approximation automaton:
```bash
cargo run approximation relabel automaton example.cfg example.classes
```

* to parse with the approximation pushdown automaton:
```bash
cargo run approximation relabel parse example.cfg example.classes
```

* approximation of a CFG (via a pushdown automaton) by a finite state automaton using a restriction of the underlying pushdown to height `k`:
```bash
cargo run approximation ptk automaton example.cfg k
```

* parse with the approximation finite state automaton:
```bash
cargo run approximation ptk parse grammar.gr k
```


## coarse-to-fine parsing

*currently being refactored*
