# A bare unhandled Failure's sink no longer retroactively applies a later `use fatal`

Real Raku decides whether an unhandled `Failure` throws under `use fatal` at
the moment the `Failure` is *constructed*, using whichever fatal state is
active then. A `Failure` that stays soft because it was made outside `use
fatal` stays soft forever after — reading it later, even from inside a scope
where fatal is now on, does not retroactively explode it:

```raku
my $f = "a".Int;      # created without fatal -> soft
{
    use fatal;
    $f;                # sunk under fatal here -- but NOT thrown
}
say 'reached';         # raku: prints this
```

mutsu previously decided at the *sink* site instead, using whatever
`self.fatal_mode` happened to be active right then — `OpCode::SinkPop` and
`OpCode::ThrowIfFailure` threw an unhandled `Failure` unconditionally, with
no `self.fatal_mode` check at all, so the example above threw in mutsu
instead of printing `reached`.

## Root cause: not fatal_mode, but a missing "bare read never sinks" exemption

Investigating further showed the bug was not really about which `fatal_mode`
snapshot to consult — it was that mutsu's sink-time checks did not
distinguish a genuinely fresh value (a method call, `Foo.new`, a function
call) from a bare variable mention (`$f;`). Real Raku's compiler recognizes a
pure variable-mention statement as "Useless use of ... in sink context" and
never actually forces/sinks it at all — confirmed by testing that `my $f =
"a".Int; $f;` does **not** throw even with no `use fatal` anywhere in the
program, and that a bare-variable tail of a `try { $f }` block does not throw
either. Meanwhile a *fresh* sink (`"a".Int;` alone, or a bare function-call
statement whose result is discarded) always throws regardless of `use
fatal`, and construction-time `use fatal` gating (already implemented at
several assignment-shaped opcodes) correctly explodes a Failure immediately
when `my $f = "a".Int;` itself executes under fatal.

The fix threads a compile-time predicate,
`Compiler::stmt_value_is_bare_container_read`, through to the VM:

- `OpCode::SinkPop` gained a second bool (`may_explode_failure`), `false`
  only for a bare `$f;`/`@a;`/`%h;` statement; every other sunk shape (method
  calls, function-call returns, the explicit `sink EXPR` prefix) keeps the
  prior unconditional-explosion behavior.
- The implicit-try tail check (`OpCode::ThrowIfFailure`, emitted for every
  block/routine body that carries a `CATCH`/`CONTROL` phaser) is now skipped
  entirely when the trailing statement is a bare container read, matching
  the same "Useless use" exemption for a `try { ...; $f }` shape.

`OpCode::ExecCall`'s own sink path (`sink_discarded_call_value`) needed no
change: it only ever receives fresh call results, never a bare variable, so
its existing unconditional explosion was already correct.

## Verification

- New regression test `t/failure-fatal-mode-creation-time.t` covers: the
  reported repro (soft Failure sunk inside a later `use fatal` scope lives),
  the same shape with no `use fatal` anywhere, the inverse (a Failure created
  *inside* `use fatal` explodes immediately at construction), the existing
  same-scope-creation-and-sink behavior (still throws), a fresh sink still
  throwing without fatal, and the explicit `sink $f;` prefix still forcing a
  bare variable. All six assertions were also verified against real `raku`.
- The existing fatal/Failure-related `t/` suite (24 files) and the roast
  files that exercise `use fatal` (`S04-exceptions/fail.t`,
  `S03-operators/assign.t`, `S02-types/whatever.t`, `S16-filehandles/open.t`,
  `S03-operators/range.t`) all continue to pass.

A related, deeper gap was found and filed separately (not fixed by this
change): `use fatal` does not yet explode a Failure constructed *inside* a
nested list/array-literal expression (`my @a = (1, "a".Int, 3);`) —
see `todo/tickets/fatal-mode-does-not-explode-failure-nested-in-list-literal.md`.
