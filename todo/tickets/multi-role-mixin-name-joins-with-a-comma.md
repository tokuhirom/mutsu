# Two mixed-in roles render as `Int+{A,B}`; raku writes `Int+{A}+{B}`

Found 2026-09-06 while working
`todo/tickets/role-mixin-name-carries-a-spurious-type-parameter.md`. Independent
of that fix and reproduces on `main` before it.

## Repro

```raku
role A { }
role B { }
say ((1 but A) but B).^name;
# raku:  Int+{A}+{B}
# mutsu: Int+{A,B}
```

One mixed-in role agrees (`Int+{A}` in both), so this is only the multi-role
join.

## Two things differ, not one

1. **The bracketing.** raku gives each composition its own `+{...}`; mutsu puts
   all of them inside one pair, comma-separated.
2. **The order.** mutsu sorts the names alphabetically —
   `role_mixin_suffix_excluding` (`src/value/types.rs`) does
   `names.sort_unstable()` with the comment "HashMap iteration order is
   non-deterministic; sort for a stable name". raku shows *application* order,
   which is the property that actually distinguishes `(1 but A) but B` from
   `(1 but B) but A`.

So a fix has to replace the sort with the real application order rather than
just changing the separator. mutsu already records that order: every composition
stamps `__mutsu_role_seq__{name}` with a monotonic
`next_instance_id()` (`src/runtime/types/roles.rs`), added precisely so
later-wins method resolution could sort by it
(`todo/tickets/mixin-role-order-not-tracked.md`). Sorting the suffix entries by
the same stamp gives a deterministic name AND the raku order.

## Scope check before changing the format

`.^name` is what `X::` messages, `.raku`, `.gist` and every introspection print,
so the spelling is load-bearing: grep `t/` and the roast whitelist for
`+{` with a comma inside before flipping it, and expect to update pins. The
single-role spelling (the overwhelmingly common case) does not change.

## Where to look

`role_mixin_suffix_excluding` in `src/value/types.rs` — the `names.sort_unstable()`
and `names.join(",")` at the end of it — plus `role_mixin_suffix_entry`, which
renders one entry and stays as it is.

## Neighbourhood to check when fixing

Three roles; a parameterised role among them (`(1 but P[Int]) but B`); the
anonymous-role marker (`VALUE_MIXIN_MARKER`) that the same function appends; the
role-punning exclusion (`role_mixin_suffix_excluding`'s `base` argument); and
`.WHAT`'s ADR-0060 composition key, which raku makes order-DEPENDENT too --
measured: with `$x = (1 but A) but B` and `$y = (1 but B) but A`,
`$x.WHAT =:= $y.WHAT` is `False` in raku and `True` in mutsu. That is a second,
consistent face of the same defect (mutsu normalizes the composition order away
in both the name and the type identity), so a fix should decide both together.
