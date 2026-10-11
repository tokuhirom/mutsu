# Arity-incomparable multi-sub candidates are ambiguous, as in rakudo

Rakudo's `is_narrower` compares no parameter types when two multi
candidates differ in both positional count and required positional count. If
neither has a slurpy, the pair is incomparable. When both land in the same
topological tier and both bind a call, rakudo raises `X::Multi::Ambiguous`,
unless a bind-time check is involved. A `where` clause or a named parameter
is such a check, and then the first declared candidate that binds wins:

```raku
multi a(Int $g, Int $e, $d = '')         { "A" }
multi a(Any $g, Any $e, Any $t, $d = '') { "B" }
a(5, 6, 1);   # X::Multi::Ambiguous
```

mutsu ranked such a pair by its lexicographic rank and answered `A`. Now that
multi-sub candidates carry rakudo's tiers (#12816), a tiered pair like this
stays incomparable within its tier. Two binders of this kind raise the
ambiguity; with a bind-time check, declaration order decides (#12795). Multi
methods still rank only the candidates that bound, so they keep the rank
order until they are tiered too.
