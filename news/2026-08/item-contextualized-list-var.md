# Item-contextualized lists expose Scalar containers through `.VAR`

The `$(...)` item contextualizer now reports a `Scalar` container through
`.VAR`, matching Raku.  Item-contextualized lists also satisfy the `Scalar`
type constraint, while ordinary lists remain non-Scalar.

The regression coverage is in `t/item-contextualized-list-var.t`.
