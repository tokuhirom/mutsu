# Logic::Ternary: local EXPORT-body operators, and augment-once semantics

Drew `Logic::Ternary` (0.0.4) from the ecosystem-dist-roulette lock board (locked
and released on [#8977](https://github.com/tokuhirom/mutsu/issues/8977)). The
distribution was `red` with all 5 baseline files dying immediately (28/164
assertions); two general interpreter bugs were fixing it exposed, and both are
now fixed.

## Local operator subs inside a `sub EXPORT` hook's own body

`Logic::Ternary`'s exports are computed dynamically: its `sub EXPORT` declares
`multi prefix:<not3>(...) is export { ... }` and friends *locally inside its
own body*, so they can close over the `use` arguments. mutsu's ADR-0087
approximation for a `sub EXPORT` module's export set
(`collect_unit_scope_routines`) only ever walked the module file's top-level
statements, never descending into the hook's own body — so `not3 5` parsed as
a plain listop call to an undeclared function and died with "Unknown
function: not3" at run time, the first thing every one of the five test files
tried to do.

Fixed by reusing the existing precise `is export` walker
(`collect_exported_subs_in`) on the `sub EXPORT` body too, alongside the
coarser unit-scope approximation — it already knows how to capture a custom
operator's precedence and associativity, which the coarse approximation does
not attempt at all.

## `augment class` inside a repeatedly-invoked sub now runs once

`Logic::Ternary` also unconditionally `augment class Any { method Ternary(...)
{...} }` from inside its `sub EXPORT` hook. mutsu re-invokes `sub EXPORT` on
every `use` of an already-loaded module (matching real Raku's own behavior),
but until this fix it also re-ran the `augment` statement as an ordinary
runtime statement each time — so the *second* `use Logic::Ternary <opt>;` in
the same process died with `X::Redeclaration` ("Package 'Any' already has a
method 'Ternary'"). `Logic::Ternary`'s own export test does this eight times
in one file.

Real Raku elaborates `augment` at compile time of the enclosing code, once,
however many times that code is later invoked — a gap already known from
`bind_compile_time_lang`'s doc comment about `sub EXPORT` running at
module-load time rather than genuine compile time. Rather than a wholesale
architectural change, this fix gives each `augment class` statement a stable
compile-time site id (the same recipe `OpCode::BeginOnceExpr` already uses: a
hash of the declaring package, source line, augmented class name and body
fingerprint) and claims it in the existing `once` store, so an already-claimed
site is skipped instead of re-applying and hitting the redeclaration check.

## Result

`Logic::Ternary` moved from 28/164 to 73/164 assertions, and the "Unknown
function" crash is gone entirely from all five files. The distribution stays
`red` overall: the dominant remaining blocker is that `True`/`False` are
hardcoded parser literals mutsu can never let a module's `sub EXPORT`
lexically shadow, filed as
[#9047](https://github.com/tokuhirom/mutsu/issues/9047) — a design question
about the parse/load pipeline, not a bounded fix, so it was filed rather than
forced through here.

Both fixes are pinned under `t/modules/import-export/` with dedicated
fixtures (`export-hook-local-prefix-operator.t`,
`augment-in-export-hook-runs-once.t`), each verified against both `mutsu` and
`raku`.
