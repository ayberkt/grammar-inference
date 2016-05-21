# grammar-inference

This is an implementation of Buszkowski and Penn's RG algorithm (1990) for AB
grammars (classical categorial grammars). Given a set of positive examples (i.e.,
correct sentences), it infers the grammar that generated those sentences.

Here's an example from Moot and Retoré (2000); consider the following sentences:

```
[\ₑ [/ₑ a man ] swims ]
[\ₑ [/ₑ a fish ] [\ₑ swims fast ] ]
```

We represent such a derivation with missing types with the `Derivation` type
defined in `Derivation.hs`. These look like follows:

```haskell
example1 :: Derivation
example1 = Node Backslash None
             (Node Slash None (Leaf None "a") (Leaf None "man"))
             (Leaf None "swims")

example2 :: Derivation
example2 = Node Backslash None
             (Node Slash None (Leaf None "a") (Leaf None "fish"))
             (Node Backslash None (Leaf None "swims") (Leaf None "fast"))
```

We simply call `learn` on the list containing these examples. In the case that
the examples unify, the grammar is returned as a `HashMap String [Type]`.

You can build with `stack build` and run these examples with `stack exec infer`.
If you want to see the generated unification problem as well, run `stack exec
infer -- --verbose`. This gives the following output:

```
I will attempt to unify the following constraints
--------------------------------------------------
swims: x5, (x1 \ S)
a: (x4 / x6), (x1 / x2)
fish: x6
man: x2
fast: (x5 \ (x4 \ S))

Solution
--------------------------------------------------
swims: (x1 \ S)
a: (x1 / x2)
fish: x2
man: x2
fast: ((x1 \ S) \ (x1 \ S))

```

# References

Buszkowski, W., & Penn, G. (1990). Categorial grammars determined from linguistic data by unification. Studia Logica, 49(4), 431-454.

Retoré, C. (2000). The logic of categorial grammars. Lecture notes, ESSLLI, 2000, 68.
Chicago
