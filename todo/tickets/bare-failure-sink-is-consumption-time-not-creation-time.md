# A bare unhandled Failure's fatal-throw decision is made at sink time, not creation time

Found while fixing the `unhandled_failure_in_list_for_fatal` closure-boundary
bug (`t/try-fatal-does-not-retroactively-flag-closure-seq.t`,
`todo/deep/vendor-real-test-module.md`'s `t/whatever-code-fixes.t` residue).

Real Raku decides whether a Failure throws under `use fatal` **at the moment
the Failure is constructed**, using whichever fatal state is active then. A
Failure that stays soft because it was made outside `use fatal` stays soft
forever after — reading it later, even from inside a scope where fatal is
now on, does not retroactively explode it:

```raku
my $f = "a".Int;      # created without fatal -> soft
{
    use fatal;
    $f;                # sunk under fatal here -- but NOT thrown
}
say 'reached';         # raku: prints this
```

mutsu instead decides at the **sink site**, using whatever `self.fatal_mode`
happens to be *right now* — `failure_to_runtime_error_if_unhandled` is called
unconditionally (not gated on `self.fatal_mode` at all — it always throws an
unhandled Failure on sink) — so the example above throws in mutsu instead of
printing `reached`. This is unrelated to the list-descend bug just fixed
(`unhandled_failure_in_list_for_fatal`, which *is* fatal-gated): this is the
unconditional bare-Failure check at `OpCode::SinkPop` / `OpCode::ThrowIfFailure`
/ `sink_discarded_call_value` (`failure_to_runtime_error_if_unhandled`, no
`self.fatal_mode` involved).

## Why this is bigger than a one-line fix

Real Raku's actual mechanism is that `Failure.new` conditionally throws
**immediately at construction** when fatal is active then — there is no
separate "check again later" step at all. mutsu's architecture is the
opposite: every `.Int`/`.Num`/... coercion site (~33 call sites building a
`Failure` instance, spread across `dispatch_core_coerce.rs`,
`error_construct.rs`, `runtime/utils.rs`, and more — see
`grep -rn 'make_instance(Symbol::intern("Failure")'`) always builds a soft
Failure, with zero knowledge of `fatal_mode`, and defers the decision to a
scattered set of consumption-time checks (`SinkPop`, `SinkPopAssign`,
`ThrowIfFailure`, `ExecCall`'s sink, method-dispatch coercion sites, ...).

Matching real semantics exactly would mean stamping each Failure with the
`fatal_mode` active at its own creation (mirroring the existing
`captured_fatal_mode` pattern already used for closures,
`vm_closure_dispatch.rs`) and having every consumption-time check read that
stamp instead of the ambient `self.fatal_mode` — a much larger change,
touching every Failure-construction call site, not scoped to this ticket.

## Minimal repro

```raku
my $f = "a".Int;
{
    use fatal;
    $f;
}
say 'reached';
```

`raku`: prints `reached` (with a "Useless use of $f in sink context"
warning). `mutsu` (current, both native and `MUTSU_REAL_TEST=1`): dies before
printing `reached`.

## Note: this is a *pre-existing* gap, not introduced by the list-descend fix

Confirmed via `git stash` + rebuild that this reproduces identically on
`main` before that fix landed — the two bugs are independent; only the
list-wrapped (`use fatal`-gated) one was in scope for that PR.
