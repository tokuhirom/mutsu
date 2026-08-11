# Regex alternation `<...>` mis-split, missing `.^nativesize`/`.^unsigned`, and native-int params silently uninitialized

Continuing the Cro::HTTP campaign, re-measuring `t/http-router.rakutest`
after the ADR-0022 Slice 3 milestone (83/83) turned up a second wave of
failures once the suite's native-int route parameter tests
(`int8`/`uint8`/.../`int64`/`uint64`, 88 more subtests than the previous
83/83 count covered) were exercised: first a hard crash building the route
table, then near-total 404s, then 8 stray warnings. Three independent,
general-purpose interpreter bugs were involved — none of them Cro-specific.

## `ClassHOW` was missing `.^nativesize` / `.^unsigned`

`Cro::HTTP::Router`'s route compiler computes each native-int parameter's
bounds via `$type.^nativesize` and `$type.^unsigned` (Rakudo's `ClassHOW`
introspection for native types). mutsu had neither method, so building any
route with a bounds-checked native-int parameter threw "No such method
'nativesize'" and aborted the whole route table. Added both to
`dispatch_classhow_method` (`src/runtime/methods_classhow_dispatch.rs`),
backed by the existing `native_types::native_type_bits` /
`is_signed_native` tables, and whitelisted them in
`Self::is_classhow_method` (`src/runtime/methods_native_bypass.rs`) so the
`.^meta_method` dispatch path reaches them at all.

## A code assertion's `<=`/`>=` desynchronized alternation splitting

With the crash above fixed, every native-int route still 404'd. Isolated to
a regex bug with no Cro dependency at all:

```raku
say "/x" ~~ / ^ [ '/' 'x' <?{ 1 <= 127 }> | <!> ] $ /;   # raku: True, mutsu: False
```

`split_top_level_alternation` (`src/runtime/regex_parse_ltm.rs`) finds a
`[ A | B ]` group's top-level `|` by tracking `<`/`>` depth so it can tell a
real alternation bar from one buried inside a `<...>` assertion. It did not
know that a code assertion's body (`<?{ ... }>`) is arbitrary Raku code that
may contain unbalanced `<`/`>` from ordinary operators. The `<` in `<=`
incremented the depth counter with no matching `>` to close it, so the
counter never returned to zero and the `|` that followed the assertion was
never recognized as a separator — the whole `[ ... | ... ]` collapsed into
one un-split, unmatchable branch. Added `consume_code_assertion_verbatim`,
which recognizes a code-assertion/closure-interpolation opener (`<?{`,
`<!{`, `<{`) and consumes it as one opaque unit via brace-depth tracking
(mirroring the real `CodeAssertion` scanner in `regex_parse_core.rs`,
which never looked at `<`/`>` in the first place) instead of feeding its
characters through the generic angle-depth counter.

## An unpassed native-int parameter had no zero default

With route matching fixed, every route whose native-int parameter used the
Cro-idiomatic *omit the query string entirely* shape (`get -> 'id_int8',
int8 :$id { ... }`, called at `/id_int8` with no `?id=`) failed with "Use
of uninitialized value ... in string context" instead of getting the
native `0`:

```raku
sub f(int8 :$id) { say "id=$id" }
f();   # raku: "id=0", mutsu (before): warns and prints "id="
```

`missing_optional_param_value` (`src/runtime/types/mod.rs`) is the single
shared helper every unpassed optional/named parameter binds through
(9 call sites: named/positional binding, method dispatch, the VM's
light-typed call fast path, and the compiler's default-folding). It always
produced a `Value::package(...)` type object for a typed constraint,
correct for object types (`Int` unpassed is the `Int` type object) but
wrong for native ints, which have no undefined state at all — `my int8
$x;` is already `0`, never a type object. Added a native-int check ahead
of the type-object fallback, mirroring how `wrap_native_int_for_binding`
already special-cases native ints on the *passed* path.

## Result

`t/http-router.rakutest` (vendored `Cro::HTTP::Router` test suite, run via
`bash tmp/cro-suite-run.sh http`): all 171 subtests that run now pass. The
suite still exits non-zero because of one remaining, already-tracked issue —
`todo/tickets/parameter-type-not-nominalized-for-user-subsets.md`
(`Parameter.type` for a user-declared `subset ... of Str` isn't
nominalized to `Str`, so the route compiler's `$type =:= Str` check
doesn't recognize it) — which is unrelated to the three bugs above and
needs its own plumbing work.

`make test` (28368 tests across 3029 files) and the full `S05-*` /
`integration/*` whitelisted roast subsets are green; three new local
regression tests were added
(`t/native-int-classhow-nativesize-unsigned.t`,
`t/regex-alternation-code-assertion-relop.t`,
`t/missing-optional-native-int-param-defaults-zero.t`).
