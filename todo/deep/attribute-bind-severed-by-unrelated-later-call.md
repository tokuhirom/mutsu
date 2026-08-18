# An attribute `:=` bind is silently severed by an unrelated later call in the same dynamic chain

Found investigating `t/has-attr-binding.t`'s regression under `MUTSU_REAL_TEST=1`
(`todo/deep/vendor-real-test-module.md`'s `t/` residue list, 2026-08-18 sweep).

`has $.x; method bind { $!x := $var }` binds the attribute `$!x` to an outer
`$var`'s container. Reading `$obj.x` after `$var` is later reassigned should
reflect the new value (it is a genuine alias, not a copy) — and it does, as
long as nothing else runs in between. But if an *unrelated* subsequent call
touches a completely different (module-lexical) variable via a method call,
the bind is silently severed: `$obj.x` freezes at the value `$var` held at
bind time instead of tracking it.

## Minimal-so-far repro (does NOT reduce to plain user code — see below)

```raku
use Test;   # or the vendored real Test.rakumod, MUTSU_REAL_TEST=1
plan 1;

my $var = 100;
my class Klass2 { has $.x; method bind { $!x := $var } }

my $obj = Klass2.new;
lives-ok { $obj.bind() }, 'binding lives';
say $obj.x;      # 100, correct either way
$var = 200;
say $obj.x;      # raku: 200.  mutsu (native or real Test): 100.
```

Confirmed with `t/has-attr-binding.t` under the vendored real `Test.rakumod`
(`MUTSU_REAL_TEST=1 target/release/mutsu t/has-attr-binding.t`, test 6
"binding $!x tracks source changes") — this is the *native* Test provider's
own `lives-ok`/lives implementation too, not something specific to the real
module; `t/has-attr-binding.t` itself already passes under the *native*
provider only because that provider's `lives-ok` happens not to trigger the
sensitive shape (see below).

## What was isolated (bisected against a trimmed copy of `Test.rakumod`, not by guessing)

Using `tmp/core/Test2.rakumod` (a `unit module Test2;`-renamed copy of the
vendored real module, see `todo/deep/vendor-real-test-module.md`'s
reproduction recipe) and progressively trimming `lives-ok`/`proclaim`:

- `try { }` around `$code()` is **not** required — calling `$code()` directly
  reproduces it too.
- `proclaim`'s `Bool(Mu)` coercion signature, `$desc is copy`, `_init_io`,
  and the TAP-counter increment are **not** required.
- The trigger is: **after** `$obj.bind()` runs (inside the block passed to
  `lives-ok`), `lives-ok` makes **any further call** to a **separate sub**
  that itself calls **any method** on a **module-lexical variable** (e.g.
  `$indents.WHAT`, where `$indents` is `Test.rakumod`'s own `my $indents = ""`
  file-scope lexical — completely unrelated to `$obj`/`$var`). With that
  second call removed (or reduced to touch no module lexical / do no method
  call), the bind survives correctly.
- `$obj.WHERE` is **unchanged** across the corruption — this is not an
  instance-identity swap. The instance is the same object; its `!x`
  attribute slot itself has stopped being an alias to `$var`'s container.

## Why this did NOT reduce to a small, Test-module-free repro (yet)

A hand-authored module matching the same shape — a `my`-scoped module lexical,
a plain (or `multi`) exported sub that method-calls it, called as a second
statement after a `Callable $code`-parameter sub that ran the binding block —
did **not** reproduce it, even copying in over a dozen dummy `my` globals to
match `Test.rakumod`'s own file-scope variable count. So the trigger depends
on some other detail of the real module not yet isolated (candidates not yet
ruled out: the specific *number/order* of other subs/multis already declared
in the same compunit before `lives-ok`, whether the class `Klass2` and the
`use`d module happen to share some registry slot, or a `Callable`
type-constrained parameter binding path that behaves differently once many
other candidates are registered against the same short name pool).

## Suspected mechanism (not confirmed — worth checking first)

`$!x := $var` compiles through the general `AssignOp::Bind` path
(`src/compiler/stmt.rs`, the `Stmt::Assign { op: Bind, .. }` arm) with
`effective_name = "!x"`, `MarkScalarBindContext`, then `SetGlobal`/`SetLocal`
— attribute reads/writes inside a method body go through the env under a
`"!name"` key, and get reconciled back into the actual `Instance`'s attribute
`HashMap` at some point (the `env_dirty` dual-store mechanism CLAUDE.md's
"Execution pipeline" section names as still-being-paid-down tree-walk-era
debt). The working theory is that this attribute writeback is not immediate
and not properly preserving the *bound* (shared-cell) nature of `!x` when it
eventually runs — and that an unrelated call touching *other* env state
(specifically one that does a method dispatch, which walks/rebuilds parts of
the env) triggers that writeback early, using a plain-value copy instead of
the shared cell. Not verified by breakpoint/watchpoint yet — the next step
for whoever picks this up is a `rust-gdb` watchpoint on the `Instance`
attribute `HashMap` entry for `"!x"` (or wherever attribute writeback
actually lives — grep `env_dirty`, `writeback`, and the `Stmt::Assign` /
`MarkScalarBindContext` compiled call sites) across the trimmed-module repro
above, to catch the exact write that severs the bind.

## Ruled out (checked with `rust-gdb -batch` breakpoints, not printf)

- `write_self_attr_cell` (`src/vm/vm_var_assign_computed_attr.rs`) is **never
  called at all** during the whole repro (breakpoint set for the entire run,
  zero hits) — so neither the bind itself nor its later corruption goes
  through that function. That function's own doc / neighbours
  (`read_self_attr_cell`, `sync_attr_local_from_cell`,
  `mirror_attr_local_to_cell`) are for *inside-a-method* read-modify-write
  syncing between a per-frame local slot and the instance's live cell — not
  relevant to `$obj.x`'s external accessor read/write, so that whole
  mechanism looks like a dead end for this bug specifically.
- `$obj.WHERE` is unchanged, ruling out an instance-identity swap (see
  above).
- The `ATTR_ALIAS_META_PREFIX` / `__mutsu_attr_alias::` mechanism
  (`vm_method_dispatch.rs`) is for *sigilless* attribute parameters
  (`method f(\a) {...}`), not `$!x := $var` inside a method body — not
  applicable here.

Next step for whoever picks this up: find where `AssignOp::Bind` targeting an
attribute name (`effective_name` starting with `!`, `MarkScalarBindContext` +
`SetGlobal`/`SetLocal`, `src/compiler/stmt.rs` around the `Stmt::Assign`
arm) is *executed* at the VM level (grep the corresponding `OpCode::SetGlobal`
/ `SetLocal` handler for a bind-mode branch that targets `InstanceAttrs`), and
watch that map entry's value directly (`rust-gdb`, break where `$obj.x`'s
accessor reads the map, print the entry's address, then `watch` it for the
whole run) rather than guessing which higher-level function is responsible.

## Why this matters beyond `has-attr-binding.t`

Any real-world module doing "bind an attribute to a caller-supplied
container" (a common pattern for lazy/proxy attributes) inside a method that
is itself called from within a *framework* callback (a test harness, an event
handler, ...) is at risk of exactly this silent divergence — the bug is not
specific to `Test.rakumod`, `Test.rakumod` merely happens to be a real-world
module big enough and shaped right to trigger it via `lives-ok`.
