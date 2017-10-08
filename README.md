# rustomata
Framework for (weighted) automata with storage

## example usage
* create a grammar file
```bash
cat <<EOF > grammar.gr
initial: [S]
S → [[Var 0 0, Var 1 0, Var 0 1, Var 1 1]] (A, B)   # 1
A → [[T a, Var 0 0],  [T c, Var 0 1]     ] (A   )   # 0.5
A → [[],  []                             ] (    )   # 0.5
B → [[T b, Var 0 0],  [T d, Var 0 1]     ] (B   )   # 0.5
B → [[],  []                             ] (    )   # 0.5
EOF
```
* construct a tree-stack automaton from the given grammar,
print out that automaton
```bash
cargo run mcfg automaton grammar.gr
```
* construct a tree-stack automaton from the given grammar and recognise the word "a a b c c d",
print out a best (maximum weight) accepting configuration (if there is one)
```bash
echo "a a b c c d" | cargo run mcfg parse grammar.gr
```

## grammar format by example
The given grammar must be a multiple context-free grammar (MCFG) to construct a tree-stack automaton or a context-free grammar (CFG) to construct a push-down automaton.
MCFGs are expressively equivalent to linear context-free rewriting systems (LCFRSs) and simple range concatenation grammars (sRCG).
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

S → [[Var 0 0, Var 1 0, Var 0 1, Var 1 1]] (A, B)   # 1
A → [[T a, Var 0 0],  [T c, Var 0 1]     ] (A   )   # 0.5
A → [[],  []                             ] (    )   # 0.5
B → [[T b, Var 0 0],  [T d, Var 0 1]     ] (B   )   # 0.5
B → [[],  []                             ] (    )   # 0.5
```

* an example of an CFG:
```
initial non-terminals: S

S→ A        with weight 1
A→ a A B    with weight 0.6
A→ a        with weight 0.4
B→ b        with weight 1

```

* the same grammar in rustomata's notation:

```
initial: [S]

S → [Nt A             ] # 1
A → [T a, Nt A, Nt B  ] # 0.6
A → [T a              ] # 0.4
B → [T b              ] # 1

```
## recognition functionality
To create an tree-stack automaton from an MCFG and print it use
```bash
cargo run mcfg automaton grammar.gr
```

To recognize from stdin using a tree-stack automaton use
```bash
cargo run mcfg parse grammar.gr
```

The same functions for push-down automata and CFG are
```bash
cargo run cfg automaton grammar.gr
```

```bash
cargo run cfg parse grammar.gr
```
## approximation
Rustomata contains several approximation strategies, allowing the transformation of automata with storage into other automata with storage. Available are

* Transform Tree-Stack (TTS) transforms a tree-stack automata into a push-down automata.
```bash
cargo run approximation tts automaton grammar.gr
```
Transforms an MCFG into a tree-stack automaton, then transforms this automaton into a push-down automaton and prints both.

```bash
cargo run approximation tts parse grammar.gr
```
Creates both automata and recognizes words from stdin using the latter.

* Relabel transforms a push-down automata into another push-down automata, where the non-terminals are replabelled using an equivalence-relation. Equivalence relations are defined by their equivalence classes in an extra file looking like this:
```
S [S    ]
N [A, B ]
R [*    ]
```

with `*` matching every non-terminal not included.

```bash
cargo run approximation relabel automaton grammar.gr eq.classes
```
Transforms a CFG into a push-down automaton, then transforms this automaton into a push-down automaton using `eq.classes` and prints both.

```bash
cargo run approximation relabel parse grammar.gr eq.classes
```
Creates both automata and recognizes words from stdin using the latter.

* Topk transforms a push-down automata into a push-down automata only containing stacks of height small or equal `k`.

```bash
cargo run approximation relabel automaton grammar.gr k
```
Transforms a CFG into a push-down automaton, then transforms this automaton into a push-down automaton with max height `k`.

```bash
cargo run approximation relabel parse grammar.gr k
```
Creates both automata and recognizes words from stdin using the latter.

## coarse-to-fine recognition
Rustomata also allows for recognition using a coarse-to-fine scheme, where we create multiple automata using approximation and recognise words on the coarsest automata, while checking the results using the finer automata.

```bash
cargo run coarse-to-fine mcfg automaton grammar.gr eq.classes k
```

creates a tree-stack automaton from an MCFG and then three push-down automata using the implemented approximation functions. Prints all automata. Uses `k` and `eq.classes` for these approximations.

```bash
cargo run coarse-to-fine mcfg parse grammar.gr eq.classes k
```

creates all automata and then recognizes the words in stdin using coarse-to-fine recognition.

The same functionality can be achieved for CFG using

```bash
cargo run coarse-to-fine mcfg automaton grammar.gr eq.classes k
```

```bash
cargo run coarse-to-fine mcfg parse grammar.gr eq.classes k
```

but this scheme does not use TTS.
