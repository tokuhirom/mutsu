# A `when X::Deeply::Qualified::ClassName { ... }` inside `Cro::HTTP::Router.rakumod`'s CATCH block fails to parse ("needs parens to avoid gobbling block")

## TL;DR

Loading the real, vendored `Cro::HTTP::Router` module fails to parse with:

```
Failed to parse module 'Cro::HTTP::Router': X::Comp::Group: Function
'X::Cro::BodyParserSelector::NoneApplicable' needs parens to avoid gobbling
block (or perhaps it's a class that's not declared or available in this
scope?)
Missing block (apparently claimed by 'X::Cro::BodyParserSelector::NoneApplicable')
```

This is the sole failure in `roast`-independent Cro suite file
`http-router-plugin.rakutest` (Cro::HTTP suite, see `tmp/cro-suite-run.sh
http` — as of 2026-08-10, 26/35 files fully green, this is one of the
remaining 9).

## Minimal deterministic repro (no roast/CI dependency)

```
cargo build   # debug is fine
target/debug/mutsu -I <CRO_HTTP_CHECKOUT>/lib -e 'use Cro::HTTP::Router;'
```

Fails 100% of the time (confirmed with `~/.cache/mutsu/precomp` freshly
cleared, so this is NOT the module-resolution-flaky ticket
— `todo/tickets/module-resolution-flaky-with-multiple-I-paths-and-global-precomp-cache.md`
— that one is a DIFFERENT, intermittent bug that also happens to surface in
Cro but is unrelated to this one, which is 100% deterministic).

`raku -I <CRO_HTTP_CHECKOUT>/lib -e 'use Cro::HTTP::Router;'` succeeds
(module loads cleanly).

The specific offending source line
(`<CRO_HTTP_CHECKOUT>/lib/Cro/HTTP/Router.rakumod:228`):

```raku
CATCH {
    when X::Cro::HTTP::Router::NoRequestBodyMatch {  # declared IN THIS FILE (line 30) — parses fine
        $response.status = 400;
    }
    when X::Cro::BodyParserSelector::NoneApplicable {  # <-- line 228, declared in a DIFFERENT, `use`d module — fails
        $response.status = 400;
    }
    ...
}
```

`X::Cro::BodyParserSelector::NoneApplicable` is declared as a bare
`class X::Cro::BodyParserSelector::NoneApplicable is Exception { ... }` in
`Cro::BodyParserSelector.rakumod` (a separate module, transitively `use`d —
`Router.rakumod` line 3 is `use Cro::BodyParserSelector;`). In real Raku, a
fully-package-qualified `class X::A::B::C { ... }` declaration is globally
addressable by that name once its declaring module has been loaded,
regardless of export — this is standard behavior for the conventional `X::`
exception-class namespace, not something Cro does unusually.

## What was tried and ruled out (do not re-attempt these — all reproduced
correctly against `raku` but did NOT reproduce the mutsu bug)

Several synthetic repros were built attempting to isolate the trigger, none
reproduced the failure under mutsu (all printed `caught`, matching `raku`):

1. A minimal two-file case: module `Foo::Errors` declares
   `class X::Foo::Errors::Bad is Exception {}`; a caller does `use
   Foo::Errors；when X::Foo::Errors::Bad { ... }` inside a `CATCH`. **Passes.**
   (See `tmp/when-cross-module/Foo/Errors.rakumod` +
   `tmp/when-cross-module-main.raku` for the harness, reusable for further
   attempts — edit the `test()` body.)
2. Same, but with an earlier `when` clause for a LOCALLY-declared exception
   type before the cross-module one (mirrors Router.rakumod's two
   consecutive `when`s, one local one foreign). **Passes.**
3. Same, but with a `my $callback = -> { ... }` block-valued declaration
   with NO trailing semicolon immediately followed by a `for @x -> $y {
   ... }` loop before the `CATCH` (mirrors Router.rakumod's exact
   punctuation right above the `CATCH`, in case a statement-boundary
   mis-parse earlier in the block was corrupting later symbol-table state).
   **Passes.**
4. Copying the REAL `Cro::BodyParserSelector.rakumod` (and its own
   dependencies `Cro::BodyParser.rakumod`, `Cro::MessageWithBody.rakumod`,
   `Cro::Message.rakumod`, all copied verbatim from the vendored CORE
   checkout into an isolated `tmp/shadow-bisect/lib/`) plus a trimmed
   caller doing `use Cro::BodyParserSelector; ... when
   X::Cro::BodyParserSelector::NoneApplicable { ... }`. **Passes** — so the
   exact real declaring module, used in isolation, is not sufficient to
   trigger the bug either.
5. Confirmed the failure does NOT require the full 8-path Cro `-I` list —
   a single `-I <CRO_HTTP_CHECKOUT>/lib` (no explicit `-I` for CORE, where
   `Cro::BodyParserSelector` actually lives) still fails with the exact
   same "needs parens" message. This is suspicious on its own (how does
   `use Cro::BodyParserSelector;`, which lives in CORE's lib, resolve at
   all with only HTTP's lib on the search path?) and was not further
   chased down in this session — worth checking with `MUTSU_TRACE=module`
   or similar whether the `use` actually succeeds silently via some other
   resolution path (bundled battery? stale precomp entry from an earlier
   run in the same session despite the cache clear — verify the clear
   actually ran before, not concurrently with, a background process from
   another session on this shared machine) before assuming module loading
   genuinely succeeded.

## Why this is a ticket, not resolved in-session

The real trigger requires the FULL, real `Cro::HTTP::Router.rakumod` (1636
lines, 16 `use` statements) in its real position within the real Cro::HTTP
checkout — every attempt to shrink it while preserving the failure did not
reproduce. This points at either (a) an interaction between several of
Router.rakumod's OTHER 15 `use`d modules and the specific one declaring
this exception class (something upstream pollutes or fails to populate the
parser's known-type registry only when enough OTHER modules are also being
loaded/parsed in the same compilation), or (b) something order/position
dependent within the 1636-line file itself that a smaller synthetic file
cannot trigger (e.g. a fixed-size symbol-table/cache eviction, an index
that only misbehaves past a certain declaration count). Distinguishing (a)
from (b) needs either `rust-gdb` breakpoints on the parser's type-registry
lookup (`grep` for wherever `needs parens to avoid gobbling block` /
"X::Comp::Group" originates and whatever populates the "known type" check
consulted there) while running the REAL file, or a careful line-by-line
shadow-bisect of the real `Router.rakumod` (progressively deleting method
bodies unrelated to `!invoke-internal` while re-checking the parse error
persists) rather than building up a synthetic repro from scratch.

## Verification (once fixed)

- `target/debug/mutsu -I <CRO_HTTP_CHECKOUT>/lib -e 'use Cro::HTTP::Router;'`
  should succeed silently (matching `raku`).
- `tmp/cro-suite-run.sh http`'s `http-router-plugin.rakutest` row should
  show `notok=0`.
