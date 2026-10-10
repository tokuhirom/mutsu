# ADR-12529 phase 0: name-resolution pins and counters

ADR-12529 moves mutsu's name resolution from the caller's env chain to the
code's lexical outer scope. Its first phase adds the tests and counters the
later phases are measured against, and changes no behavior.

`t/vm/scope/lexical-name-resolution-static-link.t` has one case for each kind
of name in the ADR's resolution table: lexicals, routine names, `my` types,
dynamic variables, `CALLER::`, `DYNAMIC::`, `OUTER::`, `MY::`, `LEXICAL::`,
`EVAL` and symbolic lookup. All 21 cases pass under rakudo.

Six of them fail in mutsu today. Each is marked `todo` with the phase that
fixes it. Two of the six were not known before:

- A closure that captured a `my sub` calls the global sub of the same name
  once the scope has ended.
- `EVAL` inside a closure reads its caller's same-named lexical instead of the
  closure's own.

With `MUTSU_VM_STATS=1`, a new `name-resolution` line reports:

- the scoped overlays that calls chain over their caller's env;
- the lookups that walk that chain, and how many tiers each one visits;
- the closure captures, with the entries and layers each one copies.

The walk counts are taken in debug builds only. In a release build the
instrumentation costs nothing measurable.
