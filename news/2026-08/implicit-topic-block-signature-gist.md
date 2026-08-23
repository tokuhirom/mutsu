# Bare blocks now gist their implicit topic parameter correctly

The `.signature` gist of a bare `{;}` block now matches Rakudo:

```raku
(;; $_? is raw = OUTER::<$_>)
```

The synthetic topic parameter was previously rendered as `($$_?)`, losing its
`is raw` trait, outer-topic default, and invocant separator. Explicitly empty
pointy blocks retain their `()` signature.
