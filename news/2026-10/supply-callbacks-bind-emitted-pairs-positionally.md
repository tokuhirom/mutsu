# Supply callbacks bind an emitted Pair positionally, and derived supplies keep a Preserving backlog

`MoarVM::Remote`'s test helper builds its output stream as
`$supply.grep({ .key eq "stdout" }).map(*.value).Channel` over a
`Supplier::Preserving` that emits `"stdout" => $line` Pairs. Under mutsu it died
with `No such method 'value' for invocant of type 'Str'` (#8825). Two
independent bugs were behind it, and neither needed threads or a `react` to
reproduce.

**An emitted Pair reached the callback as a named argument.** mutsu tells a
call-site named argument from a positional pair value by the `Value` variant
(`Pair` vs `ValuePair`, see `pair_as_positional`). `.sort`, `.first` and the
list `.map` already convert, but every Supply callback path handed the emitted
value over unconverted. A block with a plain positional name (`-> $p { }`)
happened to work, but a WhateverCode (`*.value`) and a `-> $_ { }` block got no
positional at all and read whatever `$_` was around — in the helper, the
`given @plan.shift -> $_ is copy` topic of the enclosing loop. All of the
Supply layer's callback sites (tap, whenever, `map`, `grep`, `do`, `unique`,
`squish`, `reduce`, `produce`, `classify`, `zip(:with)`, …) now go through one
helper, `call_supply_callback`, that passes emitted values positionally. The
legacy binder also counts the `_` of a WhateverCode or `-> $_` block as the
real positional parameter it is, so a positional Pair binds to it whoever the
caller is.

**A derived supply lost its Preserving backlog.** rakudo's `.map`/`.grep` taps
the source only when the derived supply is tapped, so a `Supplier::Preserving`
replays what it buffered to that tap. mutsu registers the transform eagerly,
at `.map` time, so a value emitted before the derived supply was tapped was
consumed into nowhere, and `.Channel` on a derived supply only ever looked at
the creation-time snapshot. A supply derived from a preserving source is now
preserving itself: it takes the source's backlog through the transform, keeps
what arrives while nothing taps it, replays that (and a finished source's
`done`) to its first tap, and `.Channel` takes the same backlog a `.tap` does.

Pinned by `t/concurrency/supply/supply-callback-pair-positional.t` and
`t/concurrency/supply/supplier-preserving-derived-replay.t`, both identical
under `raku`.
