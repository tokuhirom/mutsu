# Jupyter::Kernel and Jupyter::Chatbook `t/05-autocomplete` are accepted divergences

Both distributions build their autocomplete table with `(1 .. 0x1ffff).map({.chr})` and drop the
surrogates afterwards with `.uniprop`. rakudo keeps a surrogate in a `Str`; mutsu raises
`X::InvalidCodepoint` at construction (ADR-12640 rejected, #14416), so the file dies at the
first surrogate. Two `[[divergence]]` entries in `ecosystem/accepted-divergences.toml` record the
exact failure shape, and `scripts/ecosystem-sweep.py --regrade` grades both files `accepted`, so
both distributions read `green`. Nothing in the interpreter changed.
