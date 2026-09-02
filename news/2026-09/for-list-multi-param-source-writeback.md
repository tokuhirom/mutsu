# `for LIST -> \x, $value { }` writes each slot back to its own source variable

```raku
my $a;
my $b;
for $a, 1_000, $b, 1_000_000 -> \x, $value { x = $value }
say "$a $b";     # raku: 1000 1000000; mutsu printed two uninitialized-value warnings and "a= b="
```

## What was wrong — and it was worse than the filing said

The ticket recorded that the write-through "does not happen AT ALL" for a mixed
list. Measuring the adjacent shape first turned up a second, quieter bug: the
all-variables spelling did not work either, it merely *looked* like it did
through string interpolation.

```raku
my $a; my $b;
for ($a, $b) -> \x, \y { x = 9; y = 8 }
say $a.raku;   # was: (9, 8)     raku: 9
say $b.raku;   # was: Any        raku: 8
```

Both come from one place. A `for` over a list of scalar variables writes each
iteration's parameter back to the source variable at that index
(`write_back_to_source_var`), and:

* a MULTI-parameter loop chunks the source, so its `idx` counts **chunks**, not
  items. The writeback indexed `source_var_names[idx]` and read the parameter
  value from the single-parameter env slot, which for a multi-parameter loop is
  the whole chunk — so it stored `(9, 8)` over `$a` and never touched `$b`.
  `write_back_for_rw_param` had already grown the `let base = idx * arity`
  per-slot form for an `@`-array source; the scalar-list writeback had not;
* `for_iterable_var_names` returned nothing at all unless **every** element of
  the list was a plain variable, so a mixed list (`$a, 1000, $b, 1000000` — the
  ticket's actual shape) had no writeback targets whatsoever.

## The fix

`for_iterable_var_names` now returns a full-length vector with the empty name at
each non-variable position, and `write_back_to_source_vars` routes a chunked
loop to a new per-slot form that walks `rw_param_names` — the same per-slot
alias marker `write_back_for_rw_param` uses, empty for a slot whose parameter is
a plain readonly binding that cannot have been written and must not overwrite
its source. Both writeback forms now share one store helper, which keeps the
§1.5 compiler-baked-slot preference.

Pinned by `t/for-list-multi-param-write-through.t` (10 tests, green under `raku`
too), covering the all-variables and mixed spellings, a readonly slot leaving
its source alone, `is rw` parameters, a three-slot chunk (so the arity multiply
is exercised), and the single-parameter form as a regression guard.

## What is still open

The ticket's second half — that `x = $value` should raise
`X::TypeCheck::Assignment` when the source is a constrained
`my SmallInt $a` — does **not** follow from this, and the reason is not the loop:
an *undefined* typed scalar does not carry its constraint into a sigilless alias
at all, so the plain declaration form misses it too.

```
raku  -e 'subset S of Int where * < 128; my S $c;     my \y := $c; y = 1000'   # X::TypeCheck::Assignment
mutsu -e 'subset S of Int where * < 128; my S $c;     my \y := $c; y = 1000'   # X::Assignment::RO, then dies
raku/mutsu -e 'subset S of Int where * < 128; my S $c = 5; my \y := $c; y = 1000'   # both: X::TypeCheck::Assignment
```

That is filed separately as
`todo/tickets/undefined-typed-scalar-loses-its-constraint-when-aliased.md`.
