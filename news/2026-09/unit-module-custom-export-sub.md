# A `unit module`'s custom `sub EXPORT` now runs

`NativeLibs t/01-basic.t` was the first of the four bundled-library regressions
in [#7555](https://github.com/tokuhirom/mutsu/issues/7555) — the ones blocking
the vendored-`Test`-by-default flip. Under the vendored module its test 6
(`ok ::('NativeCall') !~~ Failure`) reported `MISSING`; under the native
provider it passed, but for the wrong reason. It now passes **23/23 under both
providers**, and it does so through the mechanism rakudo uses.

Four independent interpreter bugs sat between here and there. Each is general;
none is about `Test`.

## 1. A `sub EXPORT` above the `unit module` line was never called

`unit module Foo;` packages *the rest of the scope*. What precedes it stays in
the compilation unit's outer scope — and that positional rule is the whole
mechanism behind a custom export hook, because `use` looks the hook up in the
compunit's outer scope, not inside the module. Every module that has one
therefore writes it above the line, as `NativeLibs` does:

```raku
use NativeCall;
sub EXPORT(|) {
    my $exp = &trait_mod:<is>.candidates.first: { .signature ~~ :(Routine, :$native!) };
    Map.new('NativeCall' => NativeCall, '&trait_mod:<is>' => $exp.dispatcher)
}
unit module NativeLibs:auth<salortiz>:ver<0.0.9>;
```

`compile_unit` emitted the runtime package switch for the *whole* compilation
unit before the sub-hoist pass, so the hook registered as `Foo::EXPORT` and
`apply_module_export`'s `GLOBAL::EXPORT` lookup missed it. The custom export of
every `unit module` in the ecosystem was silently a no-op. The hoist is now
split at the declaration: what precedes it registers under `GLOBAL`, and the
runtime package is handed back for those statements' own execution, so the
declaration re-establishes it when execution reaches it. Measured against
rakudo: above the line runs, below it does not.

## 2. `.dispatcher` answered the candidate, not the proto

Raku generates a dispatcher even for a `multi` written with no `proto`, and
`.dispatcher` on a candidate returns it. mutsu answered the candidate itself —
and, for a `Sub`-shaped value, an ADR-0070 `<composed-method:dispatcher>` stub,
because nothing handled the method at all on that shape. Either way
`.dispatcher.candidates` reported a single entry, which silently narrows a whole
multi to the one candidate that was introspected. That is exactly what the
`Map.new('&name' => $candidate.dispatcher)` re-export idiom does, so
`NativeLibs` handed its importers a one-candidate `&trait_mod:<is>`. `Routine`
handles now also answer `.is_dispatcher`/`.multi`, which the fixed
`.dispatcher` makes reachable.

## 3. NativeCall exported a content-free `trait_mod:<is>`

Rakudo's `NativeCall.rakumod` declares its four trait candidates
(`:$native!`, `:$symbol!`, `:$nativeconv!`, `:$encoded!`) as ordinary multis and
exports them, which is what makes `&trait_mod:<is>.candidates` introspectable.
mutsu registered only a bare exported *name*, so `.candidates` answered one
entry with no signature, `.first` gave `Nil`, and the module died on
`Any.dispatcher`. The four candidates are now spliced in as a prelude, gated on
the compunit both using NativeCall and naming `trait_mod:<is>` itself — the
gate matters, because their mere presence flips
`has_proto`/`has_multi_candidates` and would route every other unknown `is`
trait in the file through custom dispatch. Their bodies are empty on purpose:
mutsu applies all four natively at declaration time
(`register_native_call_sub`), and `registration_sub.rs` already excludes them
from the custom-`trait_mod:<is>` loop for that reason, so these candidates are
never what applies the trait — they exist to make the export surface
introspectable, as rakudo's do.

Two lifetime bugs surfaced with them. A prelude splice declared as a `multi`
registers under arity-suffixed keys (`GLOBAL::name/2`, and the chained `__mN`
slots), never under the bare `GLOBAL::name` the single-sub path records, so the
prelude bookkeeping missed it entirely: `reinstate_module_functions` dropped the
candidates as importer-scoped `GLOBAL::` aliases the first time a block scope
unwound. And Raku re-runs `sub EXPORT` on *every* later import of an
already-loaded module, by which time the importer's scope holds none of the
module's own lexicals — `Map.new('NativeCall' => NativeCall, …)` quietly
degraded to the bareword string `"NativeCall"`, which then shadowed the real
package. The remembered hook now carries the module-scope env it was first
called in, and both call paths anchor to the module's own compunit.

## 4. A `Signature` literal lost its parameters across the precompilation cache

This one is independent of `EXPORT` and reproduces in plain Raku:

```raku
# lib/SigRT.rakumod
unit module SigRT;
multi sub mm(Routine $r, :$native!) is export { 'native' }
multi sub mm(Routine $r, :$symbol!) is export { 'symbol' }
our sub probe is export {
    &mm.candidates.map({ (.signature ~~ :(Routine, :$native!)).Str }).join(",")
}
```

```
cold: True,False      # rakudo: True,False
warm: False,False
```

A `Signature` literal is an `Expr::Literal` holding a `Signature` instance, but
its structured parameter data lives in a process-global side table keyed by that
instance's id — which nothing in a serialized value can reach. A module restored
from the cache came back with a literal whose id named nothing,
`extract_sig_info` fell through to its empty-params legacy shape, and the
smartmatch went from True to False. Only the *second* run of a program sees it,
which is precisely what CI's always-cold runners cannot catch.

`SerValue::Instance` now carries the `SigInfo` alongside a `Signature`, and the
value is rebuilt whole from it on load — under a *fresh* id, so it cannot
collide with an unrelated instance holding the recorded id in this process.
`CACHE_FORMAT_VERSION` moves to 12.

## Verification

`NativeLibs`' upstream suite passes under both providers, cold and warm.
Pinned by `t/unit-module-export-sub.t`, `t/multi-candidate-dispatcher.t`,
`t/nativecall-trait-mod-is-candidates.t` and
`t/signature-literal-precomp-warm-cache.t` — all four fail on the previous
`main`, and all four pass under real `raku` unchanged.

Items 2 and 4 of #7555 were fixed earlier (#7653, #7663) and item 3 is tracked
separately as [#7667](https://github.com/tokuhirom/mutsu/issues/7667); this
closes the last one.

## A fifth bug the gate surfaced: calling an enum value

`Log::Async`'s `16-imports2.rakutest` was on the bundled-library whitelist and
started failing under the change above — but it had been passing for the wrong
reason. Its `sub EXPORT` reads `@*ARGS`, and the re-run used to see the
*importer's* copy: mutsu matched the module's `--log=` branch against the test's
`--level=trace` (with a `Use of Nil in string context` warning), which left
`$level` a `WhateverCode`. Running the hook in the module's own scope — what
rakudo does — makes `$level` the plain enum value rakudo produces there, and
the test then calls it.

Rakudo answers that call: `Lv::DEBUG(1)` is the same coercion as `Lv(1)`, both
giving `Lv::TRACE`, and an undefined value when the enum holds no such value.
mutsu implemented only the type-object half. `vm_call_on_value` already routes
an invoked `Package` to the bare-name call path — "invoking a type object is a
coercion, not a `CALL-ME` call" — but an enum *value* is a `ValueView::Enum`,
matched neither that arm nor the `Instance | Package` one, and fell through to
the `CALL-ME` fallback. Pinned by `t/enum-value-call-coercion.t`.

One divergence is left untouched because it predates this and belongs to the
shared coercion helper, not to the new arm: `Lv('DEBUG')` answers `Lv::DEBUG` in
mutsu where rakudo answers `Nil`. The value form now agrees with the type form,
which is the property that matters here.
