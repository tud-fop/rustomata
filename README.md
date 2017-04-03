# rustomata
Framework for (weighted) automata with storage

## example usage
* construct a tree-stack automaton from the given grammar and recognise the word "a a b c c d",
print out a best (maximum weight) accepting configuration (if there is one)
```bash
cat <<EOF > grammar.gr
initial: S
S → [[Var 0 0, Var 1 0, Var 0 1, Var 1 1]] (A, B)   # 1
A → [[T a, Var 0 0],  [T c, Var 0 1]     ] (A   )   # 0.5
A → [[],  []                             ] (    )   # 0.5
B → [[T b, Var 0 0],  [T d, Var 0 1]     ] (B   )   # 0.5
B → [[],  []                             ] (    )   # 0.5
EOF
echo "a a b c c d" | cargo run grammar.gr
```

## grammar format by example
The given grammar must be a multiple context-free grammar (MCFG).
MCFGs are expressively equivalent to linear context-free rewriting systems (LCFRSs) and simple range concatenation grammars (sRCG).
The weights are (for now) assumed to be from the algebra (ℝ₊, ⋅, 1) of non-negative reals with multiplication.

* an example of an MCFG (in the notation of an sRCG):
```
initial non-terminal: S

productions:
S(x₁y₁x₂y₂) ← A(x₁, x₂) B(y₁, y₂)  with weight 1
A(ax₁, cx₂) ← A(x₁, x₂)            with weight 0.5
A(ε, ε)     ← ε                    with weight 0.5
B(by₁, dy₂) ← B(y₁, y₂)            with weight 0.5
B(ε, ε)     ← ε                    with weight 0.5
```

* the same grammar in rustomata's notation:
```
initial: S

S → [[Var 0 0, Var 1 0, Var 0 1, Var 1 1]] (A, B)   # 1
A → [[T a, Var 0 0],  [T c, Var 0 1]     ] (A   )   # 0.5
A → [[],  []                             ] (    )   # 0.5
B → [[T b, Var 0 0],  [T d, Var 0 1]     ] (B   )   # 0.5
B → [[],  []                             ] (    )   # 0.5
```
