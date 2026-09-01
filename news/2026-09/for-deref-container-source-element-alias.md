# `for @$s` binds element containers (ADR-0045 row 39), and the ticket's root cause was wrong

ADR-0045 row 39 — `for @$s <-> $x { $x = $x + 1 }` aliasing the inner array's
elements — had been implemented, measured, and backed out, with
`todo/tickets/for-deref-container-source-promotion-breaks-nqp-type-tests.md`
recording why: promoting the topic for the `$`-tagged deref'd-container source
made CBOR::Simple encode a `Map` as an integer, so the ticket concluded that
`encode($_) for @$_` handed a `ContainerRef` to the `nqp::` layer, whose
structural type tests answer about the container rather than what it holds. The
prescribed fix was an audit of every `nqp::` op that inspects a value's shape.

That diagnosis was wrong, and the audit alone does not fix it. Decontainerizing
the whole `nqp::` boundary and re-enabling the shape left
`CBOR::Simple 04-tags.rakutest` failing exactly as before.

## What was actually happening

`ForElementAlias::ArrayIndex` carries the source's **name** and
`for_element_alias` re-resolves it on every iteration. That is deliberate for
the `@a` shape: a body that assigns the array wholesale (`@a = 7, 8`) must have
the remaining iterations alias the container it left behind.

It is wrong for the `$`-tagged shape. `for @$s` derefs `$s` exactly once to pick
the array it walks, so a later write to `$s` cannot redirect the loop — and the
name in the idiom that motivated the row is `$_`:

```raku
my &walk = -> $v {
    with $v {
        if $_ ~~ Positional { walk($_) for @$_ }
        else                { say "leaf: ", $_.raku }
    }
};
walk([[0, 2], "x"]);
```

Every nested loop rebinds the topic, so re-resolving `"$_"` for the outer loop's
second iteration aliased into the container the *inner* loop had been walking.
raku prints `0`, `2`, `"x"`; mutsu printed `0`, `2`, `2` — the inner list's
`[1]`. The `Map` in CBOR::Simple's Capture encoding was never type-tested as a
cell; the loop bound an `Int` in its place, which then took the `Numeric` arm
honestly.

## Fix

`ForElementAlias::ArrayValue(Value)` captures the array **resolved once at loop
entry** and indexes into that, for the `$`-tagged source only. The `@a` shape
keeps its by-name re-resolution and its reason.

Rows 39 and 39b lose their `todo` markers in `t/for-loop-element-alias.t`, and
two rows are added: 39c pins the recursive-walk shape above, 39d pins that
reassigning the scalar mid-loop (`for @$s <-> $x { $s = [9, 9]; ... }`) does not
redirect the alias — matching raku.

## The `nqp::` hardening is kept anyway

The back-out's partial work was worth generalizing even though it was not the
bug: `call_nqp_op` now decontainerizes its operands once at the boundary instead
of `nqp::istype` doing it alone. No `nqp::` op wants a Raku container — the
in-place mutators (`bindpos_*`, `write*`, `splice`) reach their Buf/array through
the shared `Gc` behind the value, which survives the deref — and ADR-0036/0045
hand out element containers from a widening set of producers, so the boundary is
the right place to state it once.

## Verification

- `t/for-loop-element-alias.t` 72 rows, with 39/39b un-`todo`ed and 39c/39d new.
- Full bundled-library gate: `274/297` (up from 273), `GATE PASSED` —
  `CBOR::Simple 04-tags.rakutest` is back.
- `make test` and the full whitelisted roast sweep (1435 files, 218 833 tests)
  both green.

ADR-0045's slice-4 status section is updated: row 39 is no longer carried over
to slices 5-6, and the misdiagnosis is recorded there, because the section's own
standing warning ("the hazard is type-testing a promoted value, not reading one")
is what made the wrong reading so plausible.
