# The routine-package switch stops allocating, stops interning, and stops running at all when it is a no-op

[#8686](https://github.com/tokuhirom/mutsu/issues/8686) Phase 1's second bullet: whether the
`current_package` bookkeeping every routine call performs "can cheaply no-op for a plain `sub` call".
It can, and it turned out to be most of what a light call into a module's own sub was doing besides
running the body.

## What the switch is for, and what it was costing

Every call into a routine declared in a non-`GLOBAL` package brackets its body with a
`current_package` switch, so that an unqualified name inside the body resolves against the
*declaring* package — `our $x`, a `package { my $x }` lexical, a sibling sub. Three mechanisms do it,
one per dispatch family: `enter_routine_package`/`leave_routine_package` (light and fast paths),
`CurrentPackageGuard` (the general named binder and closure dispatch), and `exec_package_scope_op`
(a `package`/`module` block body).

The first two went through the by-name `set_current_package`, which calls `Symbol::intern`. So a
light call into a module's own sub re-hashed the declaring package's name on the way in and again on
the way out — for a string whose `Symbol` `CompiledFunction::package_sym` had already interned once,
at registration. Each half also cloned a `String` out from behind the `current_package` `RwLock` and
wrote another one back, and the general binder's call site built a third with
`def_package.to_string()` purely to satisfy the guard's signature.

Worse, almost none of it was a switch at all. The dominant hot shape is a routine calling a sibling
declared in its own package — every `JSON::Fast` helper calling the next — where the entry writes the
package that is *already* current and the exit writes it back again. Closure dispatch had noticed
this and gated its own switch on `data.package != self.current_package_sym()`; the named and light
call paths had not.

## Three changes, none of which alters what is computed

**The saved package is carried as an interned `Symbol`, not an owned `String`.** Reading the current
one is then a relaxed load off the `current_package_sym` mirror rather than a lock acquisition plus a
`String` clone, and the target comes from `package_sym()`. This rests on the invariant
[#7576](https://github.com/tokuhirom/mutsu/issues/7576) records: `current_package` and its symbol
mirror never disagree — `set_current_package_with_sym` asserts it, `set_current_package_shared_sym`
derives the string *from* the symbol, and the `Interpreter` clones that build a fresh pair (a thread
snapshot, a regex scratch) copy both together. The string is therefore recoverable from the symbol
and need not be saved at all, which is why `CurrentPackageGuard` could drop its `saved_str` field. A
`debug_assert_eq!` now states that invariant on the entry path, where the change depends on it.

**Neither the switch nor its restore writes anything while the package has not moved.** The restore
stays unconditional in *effect* — if the body did move `current_package`, it is still put back
exactly as before — so no caller has to know whether the entry switched anything. That mattered for
picking the shape: the alternative, skipping the guard entirely when the packages match, would have
rested on "no routine body ever leaves `current_package` changed", which is true today (the only
unrestored `SetCurrentPackage` emit is the compunit-level `unit module`/`unit package` split; a
non-unit `package Foo { … }` block is bracketed by `PackageScope`) but is a fact about the compiler
that this code has no business depending on. Checking at the write instead is the same saving with
nothing to reason about.

**`has_routine_scope_marker` is memoized on the routine.** With the redundant writes gone, a
substring scan was all that remained of `enter_routine_package_outlined`'s own cost: the marker that
distinguishes a mangled routine-scope key (`Pkg::&sub/arity`, used for a nested sub) from a real
package name is a search for `"::&"`, and it ran per call over the whole declaring package name for
an answer fixed once the routine is compiled. `CompiledFunction::package_is_routine_scoped` caches it
in a `OnceLock` beside `package_sym`, which caches the same field's interned symbol for the same
reason. Unlike `stamp_source_file`'s reset of `source_file_sym_cache`, this memo has nothing to
invalidate: `CompiledFunction::package` is never mutated after construction.

`enter_package_guarded` and `enter_package_guarded_with_sym` collapsed into
`enter_package_guarded_sym`. All three call sites already held the `Symbol`
(`CompiledFunction::package_sym`, `SubData::package`), and two of them were spelling
`sym.as_str().to_string()` only to satisfy the old signature and then paying `Symbol::intern` to get
the symbol back.

## Measured

Release build, callgrind, 20,000 calls to a sub declared in a `unit module`. Instruction counts are
deterministic and load-independent, so these are the numbers the change was iterated against; a
wall-clock figure for a document must still come from the bench CI.

A **light**-call-eligible sub (plain scalar parameters, no traits, no native types):

| | before | after |
| --- | --- | --- |
| whole run | 160,857,510 Ir | 139,684,965 Ir (−13.16%) |
| `enter_routine_package_outlined`, exclusive | 4,640,000 (2.88%) | 520,015 (0.37%) |
| `Symbol::intern` | 45,976 calls, 6.52 M Ir (4.05%) | 5,976 calls, 0.37 M Ir (0.26%) |
| `set_current_package_with_sym` calls | 40,002 | 2 |
| `current_package` calls | 20,025 | 25 |

The same sub with `str`/`int` native parameter types, which the light-path gate rejects (#8686's
Phase 0, untouched here) so the call takes the **general named binder**:

| | before | after |
| --- | --- | --- |
| whole run | 916,680,393 Ir | 904,613,508 Ir (−1.31%) |

The gap between the two is the point: the general binder's per-call constant is dominated by things
this change does not touch, which is why #8686 splits Phase 0/2 out from Phase 1 at all. A
`JSON::Fast` `from-json` of a 100-record synthetic SPDX document is accordingly unchanged within
noise (0.456–0.509 s against a 0.459 s baseline) — its helpers take their scan position as
`int $pos is rw`, so every one of them is on the general binder.

## Pinned

`t/modules/routine-package-switch-restore.t` pins the observable consequence rather than the saving.
A switch skipped when it should not be, or a restore that rebuilt the wrong text, would make an
unqualified name inside a routine resolve against the wrong package — *silently answering with
another package's sub or `our` variable* rather than failing — so each of its 12 assertions reads a
name whose meaning depends on which package is current at that moment: the light path, the general
binder's `CurrentPackageGuard` path, cross-package round trips, and warm repeat calls through the
memoized dispatch entries. All 12 pass under rakudo.

`tests/routine_package_switch_budget.rs` is the deterministic counterpart, in the manner of
`tests/named_call_intern_budget.rs`: a call into a module sub interned **3.0 times per call before
and 1.0 after**, and the budget also asserts that the count does not grow with the declaring
package's name length, which is what the interning made it do.
`tests/named_call_intern_budget.rs` was re-measured against the pre-change tree and is identical
either way (15.999 / 53.999 / 10.018), confirming the `GLOBAL`-package paths it covers did not move.

## Still open on #8686 Phase 1

The *first* bullet — the dispatch chain re-deriving the callsite name — is untouched and is now
[#8690](https://github.com/tokuhirom/mutsu/issues/8690) with per-caller measurements. It is the
larger half: **~13 `Symbol::intern` calls per call, 3.80% of the general-binder run**, spread over
`fn_base_name_registered` (3.0/call), `bind_function_args_values_inner` (2.0),
`has_proto_cached` (2.0), `dispatch_key::with_amp_name` (2.0) and four more at 1.0 each — all
re-hashing a string constant whose `Symbol` `CompiledCode::const_sym(name_idx)` already carries, and
which is in scope at the hot call sites next to the `&str` that gets passed instead. It was kept out
of this change because it means growing a `_sym` variant chain across the dispatch graph rather than
the local substitution the package switch needed — the split #7766 unit 2 anticipated.
