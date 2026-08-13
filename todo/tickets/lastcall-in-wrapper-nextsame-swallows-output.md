# `lastcall` then `nextsame` (not `callsame`) inside a method wrapper: raku produces no further output at all, mutsu prints `Nil`

Found while fixing `lastcall-in-wrapper-callsame-dies` (now closed, see
`news/2026-08/lastcall-in-wrapper-callsame-dies.md`). That fix only covers the ticket's own
scenario, `callsame` after `lastcall` inside a method wrapper (raku: `Nil`, matched by mutsu after
the fix). Probing the `nextsame` (tail-call) sibling turned up a further divergence, not part of
that ticket's scope:

```raku
sub run() {
    class C { method m() { say "orig"; "o" } }
    C.^lookup('m').wrap(-> |c { say "wrap"; lastcall; nextsame; say "unreached" });
    say C.new.m;
}
run();
```

```
raku:  wrap                (nothing else -- "say C.new.m" itself produces no output)
mutsu: wrap
       Nil                 (the "say C.new.m" line prints Nil)
```

At the top level of a script (`-e`), raku instead dies with `Attempt to return outside of any
Routine` at the `nextsame` — also not reproduced by mutsu (mutsu returns `Nil` there too). Both
shapes suggest `nextsame`'s "return from the enclosing Routine" semantics interact with a wrapper
closure's own call frame in a way neither mutsu's tail-call encoding (`RuntimeError{
return_value: Some(Value::NIL) }`, unwinding to the nearest routine boundary) nor this
investigation fully worked out — worth a dedicated raku-verification pass before attempting a
fix, per the project's usual discipline for dispatch-chain semantics.

Not chased further here: this is a probe finding, not a scoped fix. `lastcall`-then-`nextsame`
in a plain (non-wrapper) multi context already matches raku and is pinned by
`t/lastcall-then-nextsame.t`; this ticket is specifically about the wrapper-closure case.
