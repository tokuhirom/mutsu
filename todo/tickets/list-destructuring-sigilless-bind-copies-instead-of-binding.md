# A list-destructuring sigilless bind copies instead of binding

```
raku  -e 'my ($x,$y) = 1,2; my (\a,\b) := ($x,$y); a = 10; say $x'   # 10
mutsu -e 'my ($x,$y) = 1,2; my (\a,\b) := ($x,$y); a = 10; say $x'
# Cannot assign to an immutable value
```

The **single-variable** form already works in both (`my \a := $x; a = 10` sets
`$x` to 10), and so does hand-unrolling the list form
(`my \c := $p; my \d := $q`). Only the parenthesised list form is broken.

## Root cause (measured with `--dump-ast`)

The list form desugars to

```
my @__destructure_tmp__ = [$x, $y].list;
VarDecl { name: "a", expr: Index { target: ArrayVar("__destructure_tmp__"), index: 0 } }
```

The temp array holds *copies* of `$x`/`$y`, so nothing downstream can reach the
original containers — no amount of element containerization in that temp could
make `a = 10` write to `$x`.

**The fix is in the desugar**: emit N single binds, each to its own RHS lvalue —
which is exactly the form that already works — instead of routing through a
copying temp array.

## Provenance

Filed by ADR-0040 slice 5 (2026-09-02). ADR-0040 §1.7 records this as a claim
that the originating finding
(`news/2026-09/element-itemization-lost-in-scalar-binding.md`) **misfiled** as an
element-itemization symptom: it is a desugar bug, not a container-representation
one, and the ADR deliberately did not cover it. §1.7 also notes the failure mode
changed over time — it used to no-op silently, then died with
`Cannot assign to a readonly variable`, and today says `Cannot assign to an
immutable value`. Re-verified 2026-09-02 against `raku` v2026.07.
