# `[\[&f]] 1, 2, 3` — a reduction whose operator is a bracketed Callable — does not parse

Found 2026-09-06 while sweeping the neighbourhood of the triangle-reduce
laziness fix (`news/2026-09/triangle-reduce-stays-lazy-through-an-array-assignment.md`).
Pre-existing and unrelated to it: the finite, non-lazy spelling fails the same
way.

## Repro

```raku
sub f($a, $b) { $a + $b * 2 }
say ([\[&f]] 1, 2, 3);
# raku:  (1 5 11)
# mutsu: ===SORRY!=== Error while compiling -e ... Confused.
```

The fold form is refused identically:

```raku
sub f($a, $b) { $a + $b * 2 }
say ([[&f]] 1, 2, 3);   # raku: 11   mutsu: Confused.
```

## What already works, and why this is narrow

Reducing with a **user-defined infix** is fully supported, in both the fold and
the scan form:

| Program | raku | mutsu |
|---|---|---|
| `sub infix:<myop>(\a, \b) {...}; [myop] 1,2,3` | `11` | `11` — correct |
| `sub infix:<myop>(\a, \b) {...}; [\myop] 1,2,3` | `(1 5 11)` | `(1 5 11)` — correct |
| **`sub f(...); [\[&f]] 1,2,3`** | `(1 5 11)` | **parse error** |

So the *runtime* already knows how to fold with a user routine
(`reduction_callable_for_op` / `reduction_step_with_args` in
`src/vm/vm_misc_reduction_exec.rs` exist for exactly that, and take a
`callable`). What is missing is the **syntax**: `[op]` where `op` is written as a
bracketed `Callable` term (`[&f]`, and by extension `[&[+]]`) rather than as an
operator name.

Note that `[&f] 1..5` — the same thing without the inner brackets — is a parse
error in raku too, so the bracketed spelling is the one to support, not the bare
one.

## Where to look

The reduction-metaop parse: whatever recognizes `[` ... `]` as a reduction
operator and validates the inner token against the operator table
(`KNOWN_BASE_OPS` in `exec_reduction_op` is its runtime counterpart). The inner
form here is a term, not an operator name, so it needs to compile to a call site
that hands the reduction its `callable` — which the executor already accepts.

## Neighbourhood to check when fixing

The fold (`[[&f]]`) and scan (`[\[&f]]`) forms; a `&`-sigil variable holding a
block (`my &g = -> $a, $b { ... }; [[&g]] 1,2,3`); a Callable with the wrong
arity (raku's error); `[&[+]]` (a bracketed built-in operator reference); the
`R` and `!` metaop prefixes over it; and a lazy source (`[\[&f]] 1..*`), which
must stay lazy like every other scan.
