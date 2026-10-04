# `use Test` costs a fifth fewer instructions

Every file in `t/` and roast starts with `use Test`, and loading it was most of
what a typical test file cost: a release `mutsu` ran `use Test; ok 1;` in
~34 ms against ~7 ms for an empty script. Across `t/` that is about a third of
the suite's CPU time, spent before any assertion runs.

A callgrind profile of that two-line file found several costs that grew with
the size of the module (or of the registry) for no reason, all of them on the
load path:

- **The parameter-type pre-scan read `Test.rakumod`'s source one character at
  a time and built a `String` at every position** to check whether a
  declarator keyword started there. It now compares in place and rejects any
  position that cannot start a declarator with one character comparison.
- **Exporting a multi family scanned every registered function, once per
  candidate**, resolving each interned key to an owned `String` before
  comparing it. The same "resolve to compare" pattern sat in 42 other places
  (`routine_registry_keys` among them, which runs once per declaration);
  all of them now compare `Symbol::as_str()`'s `&'static str`.
- **Two analyses rendered whole ASTs with `{:?}` and searched the text**: the
  routine metadata looked for `ArrayVar("_")` in every empty-signature body,
  and the `if` compiler did the same for every branch to decide whether it
  needs its own `@_`. Both now walk the tree with `ast_visit::legacy_args`.
  Before landing, the walk was checked against the text search on every file
  in `t/` and the roast whitelist; they agreed on all of them.
- **A sub declaration's body was hashed twice**: the decl plan's registration
  fingerprint now reuses the body fingerprint the routine metadata has already
  computed.

Measured on `use Test; ok 1;`, warm precompilation cache, release build:

| | instructions | user time (100 runs) |
| --- | ---: | ---: |
| before | 114,736,256 | 2.41 s / 2.52 s |
| after | 91,144,153 | 2.07 s / 2.02 s |

That is -20.6% instructions and about -17% user time. Wall clock moves less
(~-8%), because process start-up and system time are unchanged. Most of what
is left is compiling the module and registering its routines on every load;
that needs a structural change and is tracked in #11756.
