# `for @$s` cannot bind element containers yet: the `nqp::` layer type-tests the topic

## Status

**ADR-0045 row 39 is deferred.** The routing was implemented — `for @$s` / `for $s.list` tag their
source as `"$s"`, and `resolve_for_source_array` (`src/vm/vm_for_loop_alias.rs`) already knows how to
resolve that shape to its inner array — then measured and backed out. The row stays `todo`-marked in
`t/for-loop-element-alias.t`.

## What happens

`encode($_) for @$_` is an ordinary way to walk a nested structure, and CBOR::Simple uses exactly
that (`modules/CBOR-Simple/lib/CBOR/Simple.rakumod`, the Positional arm). Promoting the source's
elements binds the body's topic to a cell, and the body then hands that cell to the `nqp::` layer:

```raku
elsif nqp::istype($_, Associative) { ... }
```

A `ContainerRef` answers `False` to the type test, so a `Map` fell through to a later arm and was
encoded as its element count. `CBOR::Simple 04-tags.rakutest` fails its Capture round-trips
(`\(0, 2, :normalize)` encodes as `D9 6361 82 82 00 02 02` instead of
`D9 6361 82 82 00 02 A1 69 6E6F726D616C697A65 F5`), which trips the bundled-library gate.

Decontainerizing `nqp::istype` alone (done, and kept — a type test should ask about the value) is
**not sufficient**: the failure persists, because the `nqp::` ops that inspect a value structurally
are many and each reads the raw value. That breadth is the finding.

## Why the sibling shapes are fine

- `for @a` (ADR-0045 slices 1-3) promotes too, and ships. The difference is not the promotion, it is
  what the corpus does with the binding: a plain `for @a` loop body works on the element as data,
  where `for @$_` is the idiom for *recursive structure walking*, which is exactly the code that
  type-tests what it is holding before recursing.
- `.values`/`.reverse`/`.sort` (ADR-0036 slice 3 / ADR-0045 slice 4) hand out a flat list of cells
  and pass both the full roast whitelist and the bundled-library gate.

## What the fix needs

An audit of the `nqp::` ops that inspect a value's shape, decontainerizing at that boundary — the
same treatment `nqp::istype` now has. `src/runtime/nqp_ops.rs` is the single file. Until then the
`$`-tagged source keeps `write_back_for_topic_item`'s `$`-source arm, which is correct for the
direct-mutation case (`$_ .= uc for @$hdr` — Text::CSV's header munge) and only loses the deferred
one.

This is the same *class* as
`todo/tickets/pairs-element-containers-leak-through-pair-value-consumers.md` — a promoted value
reaching code that **type-tests** it rather than reads it — but a different boundary (`nqp::` ops
rather than Pair-value destructuring), so it wants its own audit.

## Reproduce

Re-add `|| source.starts_with('$')` to the source test in `plan_for_element_alias`
(`src/vm/vm_for_loop_alias.rs`), then:

```
MUTSU_BIN=target/release/mutsu bash scripts/battery-testsuite.sh CBOR::Simple
```

`04-tags.rakutest` fails 2 subtests. Note that `make test` does **not** cover this — the
bundled-library gate is a separate CI step.
