# `nextsame`'s tail-call unwind now raises a real `CX::Return`, so it targets the lexically enclosing Routine

`Interpreter::dispatch_next_candidate` (`src/runtime/builtins_dispatch_next.rs`) has a
`tail_call: bool` parameter -- true for `nextsame`/`nextwith`, false for
`callsame`/`callwith` -- and all twelve of its `tail_call` legs raised the unwind
signal by hand:

```rust
return Err(RuntimeError {
    return_value: Some(result),
    ..RuntimeError::new(""),
});
```

That is not what a real `return` raises. `RuntimeError::return_signal`
additionally sets `control: Some(Control::Return)`, and `control` is exactly
what `RuntimeError::is_return()` reads to gate the arm in
`resolution_call_sub.rs` that stamps a non-Routine block's return with its
*lexically enclosing* Routine as its unwind target. Because `nextsame`'s
hand-built signal never set `control`, it was a targetless signal: the first
frame matching `Err(e) if e.return_value.is_some()` swallowed it -- the
wrapped method call, rather than the Routine the `.wrap()` block was lexically
written inside.

```raku
sub run1() {
    class C1 { method m() { say "orig"; "o" } }
    C1.^lookup('m').wrap(-> |c { say "wrap"; nextsame; say "unreached" });
    say C1.new.m;
}
run1();
say "after";
```

Before the fix, mutsu printed `wrap / orig / o / after` (`say C1.new.m`
legitimately running and printing the wrapped call's return value). Rakudo
prints `wrap / orig / after`: `nextsame` unwinds all the way out of `run1`
(the Routine the wrapper block is nested inside), abandoning `say C1.new.m`
mid-statement.

Fixed by replacing all twelve hand-built signals with the existing
`RuntimeError::return_signal(result)` constructor, which already sets
`control: Some(Control::Return)`. Two probes fix the boundaries of the fix:
`nextsame` in non-tail position (`my $x = nextsame`) still unwinds all the way
out -- this is not about syntactic tail position, it is about whether the
invoked callable is a Routine -- and a wrapper written as `sub (|c) { ... }`
(a genuine Routine) still returns from the wrapper itself rather than
unwinding further, since `nextsame` there legitimately targets its own
enclosing Routine.

Added `t/nextsame-in-block-wrapper-returns-lexically.t` pinning all three
cases (verified against Rakudo v2026.06). The known residue where the wrapper
block has no lexically enclosing Routine at all (top-level `.wrap()`) is
tracked separately by [ADR-0050](../../docs/adr/0050-block-routine-ness-is-a-definition-site-property.md),
which is architecturally independent of this fix.
