# `self`, `::?CLASS` and `::?ROLE` live in the frame record only

A method frame's invocant, `::?CLASS` and `::?ROLE` are held by its `FrameFixtures` record, and a
closure, gather or routine value carries its creator's record (`SubData::fixtures`). No `Env` key
stores them any more (#14843, ADR-14843):

- Method entry, on the fast and slow paths, writes no `self` / `?CLASS` / `?ROLE` key, and neither
  does the parameter binder for a method's implicit invocant. A method run as a function (a method
  literal, a `my method` called as a sub, a Proxy `FETCH`) takes its first argument as the record's
  invocant once its frame is pushed.
- The 17 helpers that rebound `self` / `?CLASS` around a call (role mixin application, grammar
  actions, attribute defaults, regex code atoms, ...) open a scoped record instead
  (`begin_scoped_invocant`, `begin_class_scope`, `end_scoped_fixtures`), and the scope is cut back
  on every exit.
- A CONTROL handler run inline at a warn raise site pushes the record of the frame that installed
  it, not the raise site's.
- A by-name store of `self` (a mutating call on `self`, `self{$k} = v`, `temp self.attr`) reaches the
  record's invocant (`store_invocant_by_name`); `self{...}` subscripts compile against the invocant
  expression, never a variable called `self`.
- `make check-frame-fixtures` keeps the references to the three names in the runtime layers from
  growing (`scripts/frame-fixtures-baseline.txt`).

The one remaining `self` env key is a plain routine's own `sub f($self)` parameter, a user lexical
that shares the spelling (ADR-0061).

Same-build callgrind instruction counts against `main` (`ec4c9764`, both `--release`, second run of
each; lower is better):

| benchmark | main | this change |
| --- | ---: | ---: |
| `benchmarks/bench-class.raku` | 769,131,319 | 762,809,191 (-0.8%) |
| `benchmarks/bench-ctor.raku` | 1,435,909,118 | 1,363,414,380 (-5.0%) |
| `benchmarks/bench-multi-dispatch.raku` | 2,525,306,081 | 2,517,445,358 (-0.3%) |
| method calls + closures/`map` inside a method | 573,106,834 | 561,299,552 (-2.1%) |
