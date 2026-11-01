# CompUnit::Handle.unit and CompUnit.unit answer the unit's lexical pad

`CompUnit::Util`'s `at-unit` and `unit-to-hash` read a loaded unit's lexical
pad through `$handle.unit` with `nqp::existskey`, `nqp::atkey` and
`nqp::iterator`. mutsu's `CompUnit::Handle` had no `unit` method for a loaded
unit's handle, and `CompUnit` had none at all, so `t/01-utils.rakutest` died
at its eighth test.

A loaded unit's handle now answers a pad built from what the unit left behind:
its `$=pod` (scanned from the source, not executed), `$?PACKAGE`, and its
`EXPORT` package when it has symbols. It is built on the first ask and cached,
so `at-unit($cu, '$=pod')[0] === at-unit($cu.handle, '$=pod')[0]`. `CompUnit.unit`
is its handle's pad. A handle built over a pad (`CompUnit::Handle.new(:unit(...))`)
answers that pad. `CompUnit::Handle.export-sub` is added; it answers `Nil` for a
loaded unit (a module's `sub EXPORT` hook is not kept after the load) and the
pad's `&EXPORT` for a handle built over one.

The pad is a lexpad, as in rakudo, not a `Hash`: it is typed `BOOTContext`
(`value::type_name`, the same way `PseudoStash` and `Map` are), and
`nqp::iterator` accepts it where it still refuses a high-level `Hash`.

Real `CompUnit::Util` 0.7.0, release build next to `raku`: `t/01-utils.rakutest`
goes from 7 ok to 12 ok of 17 (rakudo 17). The rest is the BEGIN-time `$*R` /
`$*CU` side (`set-in-WHO`, `steal-globalish`, `re-export`) of #15371, and
`need` of a unit with a `sub EXPORT` hook not filling its `EXPORT::DEFAULT`
stash for `capture-import`.
