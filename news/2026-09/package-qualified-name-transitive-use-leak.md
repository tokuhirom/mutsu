# A package-qualified name no longer leaks through a transitively-`use`d module

```raku
# InnerConst.rakumod: unit module InnerConst; constant INNER-CONST = 42;
# OuterConst.rakumod: use InnerConst; unit module OuterConst;

use OuterConst;   # never `use InnerConst` itself
say InnerConst::INNER-CONST;   # raku: Could not find symbol   mutsu was: 42
```

The bare-name half of this divergence (#7555 item 1(b)) closed across three
earlier slices — #7743 (imports/classes/roles), #7764 (package names), #7791
(constants/enum values) — so a *bare* `InnerConst` reference already correctly
went undeclared for an importer that only `use`d `OuterConst`. The
**package-qualified** form (`InnerConst::INNER-CONST`, `InnerConst::InnerClass`,
`InnerConst::InnerEnum`, a qualified call to one of `InnerConst`'s subs, ...)
still resolved, because mutsu's package symbols — `our`-scoped constants/vars,
classes, roles, enums — live in process-global stores keyed by their qualified
name, with no notion of who may see an entry. Rakudo installs a `use`d package
into the *importing compunit's* `MY::` only, so a compunit that never `use`d
`InnerConst` itself has no path to `InnerConst::anything`, even through a
module it did `use` that in turn `use`d `InnerConst`.

## The visibility rule turned out to have three parts, not one

The naive fix — a compunit sees `Pkg::x` only if it `use`d `Pkg` itself —
passes the issue's own repro and the local `t`/`roast` suites, but fails
`scripts/battery-testsuite.sh` (the gate that runs 17 real, vendored
distributions' own upstream test suites): it broke Cro::HTTP, HTTP::UserAgent,
XML, OpenSSL, and DBIish's mysql driver, all in the same shape. Chasing that
down against real `raku` (not just the issue's own repro) turned up two more
genuinely-permissive rules mutsu had been relying on by accident:

1. **Direct grant.** A compunit that `use`/`need`/`require`d `Pkg` directly
   may reference `Pkg::anything` — the rule the issue asks for.
2. **Package-ancestor.** Code running with current package `Foo` may
   reference `Foo::Bar::anything` even without `use`ing `Foo::Bar` itself —
   confirmed against the real, vendored `OpenSSL` module: `unit class
   OpenSSL`'s `method new` calls `OpenSSL::Ctx::SSL_CTX_new(...)` without
   ever `use`ing `OpenSSL::Ctx` (only transitively, through its own `use
   OpenSSL::SSL`). A minimal repro with an unrelated current package
   (`unit class Outer; use Bar; method probe { Baz::greet() }`, where
   `Bar.rakumod` does `use Baz;`) correctly fails under real `raku` — the
   difference is the shared name, not the `use` chain.
3. **Same top-level namespace.** A compunit that `use`d *anything* under a
   given top-level `::`-segment may reference *any other* package under that
   same segment — confirmed with `IO::Socket::Async::SSL.rakumod` (no `unit`
   declarator, so rule 2 cannot apply): a top-level `my constant ... =
   OpenSSL::Version::version_num() ...;` resolves under real `raku` even
   though the file's own `use` list never mentions `OpenSSL::Version`, only
   sibling packages (`OpenSSL`, `OpenSSL::Bio`, `OpenSSL::Ctx`, ...) under the
   same `OpenSSL::` prefix.

None of this is written down anywhere; it was reverse-engineered by running
real `raku` against both the failing vendored modules and minimal repros
until each rule's exact boundary was pinned down.

## The fix

`Interpreter::qualified_name_visible_here` checks all three, in order, at the
two chokepoints where a source-written qualified name resolves — the
bareword term path (`exec_get_bare_word_op`) and the qualified function-call
path (`exec_call_func_op_inner`):

- `current_package_is_ancestor_of` (rule 2) — a pure string ancestor-walk of
  `self.current_package()`, no bookkeeping needed.
- `package_granted_in_unit_chain` (rule 3) — a coarse top-level-segment
  membership check against `compunit_visible_packages`.
- `longest_declared_package_prefix` + `package_visible_in_unit_chain` (rule
  1, plus a compunit's unconditional view of its own declarations) — an
  exact-prefix match against `package_declaring_units`.

Two new per-compunit tables populated at the end of every successful module
load (mirroring `prelude_declaring_units`/`prelude_visible_here`, the
existing NativeCall-prelude visibility gate from #7612):

- `package_declaring_units`: **full package name** → declaring compunit.
  Deliberately NOT truncated to a first `::`-segment: a real multi-file
  distribution routinely has several unrelated compunits sharing a namespace
  prefix (`XML::Entity` and `XML::Element` are separate `unit class`-scoped
  files, both under `XML::`) — keying this by `"XML"` let whichever loaded
  first claim the whole prefix and made every sibling's OWN qualified
  self-reference to its own name look foreign (`XML::Entity.rakumod`
  referencing `XML::Entity.new` inside its own body).
- `compunit_visible_packages`: for each compunit, both the full names AND the
  top-level segments of everything it `use`d directly (rules 1 and 3) —
  populated on first load and, since a re-`use` of an already-loaded module
  skips `load_module_inner` entirely, on the already-loaded fast path too.

## Which compunit is "running" during a module load

Getting all of this right also exposed a sharper, separate bug in how mutsu
decides "which compunit is executing right now" while a module's own
top-level mainline is running. `Interpreter::executing_unit_sym` prioritizes
the topmost `routine_stack` frame — correct for an ordinary call, but wrong
for a module body, which runs via `run_block` and pushes no frame of its own.
`DBIish.install-driver('SQLite')` does a dynamic `require ::($module)` from
inside a method; the loaded module's own top-level `use NativeLibs;` then
misattributed to `install-driver`'s own compunit instead of to the module
whose mainline was actually running, because `install-driver`'s stale frame
was still on top of `routine_stack`.

`module_loading_unit_stack` now records `(compunit, routine_stack depth)`
around every module load's `run_block`; `executing_unit_sym_for_module_load`
trusts the stack's top only while that depth hasn't grown since (nothing has
been *called* since the module's mainline started), and falls back to
`executing_unit_sym` the moment a routine call — possibly into a different
compunit entirely — happens in between.

## Verification

Extends `t/module-transitive-use-does-not-leak-types.t` (the pin for the
bare-name half) with the qualified-name assertions, rather than adding a
parallel file, per the issue's own instruction. `cargo fmt`, `make lint`
(all four configurations), `make test` (41,855 assertions), `make roast`
(clean except the three documented container-only discrepancies), and
`scripts/battery-testsuite.sh` (all 17 bundled distributions' own upstream
suites — the gate that caught rules 2 and 3, which neither `make test` nor
`make roast` exercises) all pass.

Filed #7803 for an unrelated pre-existing gap found while extending the
pinned test: a `unit module`'s plain `sub NAME() is export` (as opposed to
`our sub`) is not callable via `Module::NAME()` qualified-call syntax at all,
regardless of visibility. Filed #7811 for an unrelated flaky test
(`t/whenever-callback-recompile-semantics.t` subtest 5) found while
investigating an initial CI failure — a genuine emit-order race between two
async `whenever`/`Promise.keep` continuations, reproducing on `main` too.
