# `my @n = [\~] 1..*` hangs — array assignment reifies a triangle reduce eagerly

Found by the 2026-09-06 doc-diff sweep (`raku-doc/doc/Language/operators.rakudoc:602`),
re-verified against raku v2026.07 the same day.

## Repro

```raku
my @n = [\~] 1..*;
say @n[^5];        # raku: (1 12 123 1234 12345)   mutsu: hangs (exit 124)
```

## Narrowed — it is the assignment, and only for the triangle reduce

Both halves work in isolation. Every other lazy source survives the same
assignment, and the same triangle reduce survives every other consumer:

| Program | mutsu |
|---|---|
| `say ([\~] 1..*)[^5]` | fine — subscripted directly |
| `say ([\+] 1..*)[^5]` | fine |
| `my @n = 1..*; say @n[^5]` | fine |
| `my @n = (1..*).map(*+1); say @n[^3]` | fine |
| `my @n = lazy gather { take $_ for 1..* }; say @n[^3]` | fine |
| `my @n = [\~] 1..*; say @n[^5]` | **hangs** |

So `@`-assignment does preserve laziness in general, and the triangle reduce is
lazy when consumed directly — but the `Seq` the triangle reduce hands to the
assignment is not carrying whatever marks the others as lazy, so the assignment
tries to reify it to completion.

## Where to look

The triangle-reduce (`[\op]`) producer and what kind of value it returns
(`src/vm/vm_misc_ops.rs` reduction handling), compared with what `1..*`,
`.map` over an infinite range, and `lazy gather` return. The list-assign path
(`vm_var_assign_*`) decides eagerness from that.

## Neighbourhood to check when fixing

`[\+]`, `[\*]` and a user-defined operator; `my @n := [\~] 1..*` (bind rather
than assign); assignment into `my $n =` and into a `Seq`-typed variable;
`.head(5)` / `.[^5]` / `for` over the assigned array; and whether
`[\~] (1..*).map(...)` behaves like the bare range.
