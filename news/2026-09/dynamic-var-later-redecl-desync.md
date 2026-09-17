# A later, unrelated `my $*OUT = ...` no longer desyncs an earlier `$*OUT` reassignment

Fixed issue #8652: a later, textually-unrelated `my $*OUT = ...` redeclaration
anywhere else in the same compiled unit (mainline chunk) could make an
*earlier* `$*OUT` reassignment invisible to `say`/`print`, even though the
later declaration's block had not run yet by the time the earlier `say` ran.

```raku
class Sink { method print(*@_ --> True) { } }
my $out = my $*OUT = Sink.new;
say "leaked?";
say "x";
{
    my $*OUT = Sink.new;
}
sub noop() { }
noop();
```

Expected (rakudo): no output at all — both `say`s are captured by `Sink`'s
no-op `print` method. mutsu printed `leaked?` and `x` to real stdout, as if
`$*OUT` were never reassigned.

## Root cause

`code.locals` is one flat table for the whole compiled unit. A top-level `my
$*OUT = ...` with no enclosing block compiles straight to `SetGlobal` — it
gets no local slot of its own. A block-scoped `my $*OUT = ...` elsewhere in
the *same* unit, however, does get a local slot (`SetLocalDecl`), and that
slot is named `"*OUT"` in `code.locals` regardless of where in the file the
block sits.

At frame entry, `run_inner` seeds every declared local's slot from whatever
`env` held for its bare name at that moment — for `"*OUT"` that is the
process's real stdout handle, since the block's own declaration has not run
yet. `sync_env_from_locals_declared` / `sync_env_from_locals_needed` (run
before `say`/`print`/`put`/`note` and before some calls, so a live `$*OUT`
override is visible to them) then republished *every* local whose bare name
already existed in `env` — including that still-undeclared slot — back into
`env`. Since `"*OUT"` already existed in `env` (written by the earlier,
slot-less top-level declaration), the guard was satisfied for the wrong
reason, and the stale frame-entry seed clobbered the correct value the
earlier declaration had just written.

## Fix

Added `Interpreter::dynamic_local_slot_is_live`: for a dynamic (`*`-twigil)
local, only publish its slot into `env` once its *own* declaration has
actually run in a currently open block scope, tracked via the existing
`block_declared_vars` scope stack (populated by `exec_set_var_dynamic_op`
when a `my $*x` declaration executes, and popped again at that block's
exit). Plain (non-twigil) locals are unaffected. Applied to
`sync_env_from_locals_declared`, `sync_env_from_locals_needed`, and
`sync_regex_interpolation_env_from_locals`, which shared the identical
by-name-existence guard.

Regression test: `t/vm/scope/dynamic-var-later-redecl-desync.t`.
