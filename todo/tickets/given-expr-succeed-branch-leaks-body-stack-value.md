# An expression-position `given` with a matching `when` leaks a stack value and eats a sibling argument

Found while verifying `todo/deep/when-nonmatch-value-outside-map-grep.md`
against `main` @ `4c58b5f59` (2026-08-20). Independent of that finding, and a
much smaller fix — filed separately so it does not wait on
[ADR-0052](../../docs/adr/0052-a-when-clause-produces-its-value-on-the-stack.md).

## Repro

```raku
say "A: ", (given 2 { when 2 { "two" } });   # raku: "A: two"   mutsu: "twotwo"
say "D: ", (given 3 { default { "d" } });    # raku: "D: d"     mutsu: "dd"
say "P: ", (given 1 { when 99 { "a" }; when 1 { "b" } });  # raku: "P: b"  mutsu: "bb"
```

The literal `"A: "` is silently dropped: the `given` expression pushed **two**
stack values instead of one, so `Say(2)` consumed both of them and left the
label stranded below. Any call whose argument list contains an
expression-position `given` with a matching `when`/`default` is affected, not
just `say`. A non-matching `when` is unaffected (it pushes nothing today —
that is the ADR-0052 finding).

## Root cause

`exec_when_op` (`src/vm/vm_given_when_ops.rs:447-454`) does not *pop* the
matched body's value off the stack; it peeks it (`self.stack.last().cloned()`)
into the `succeed` signal's `return_value`. The value therefore exists twice,
and each enclosing construct is responsible for dropping the stack copy.

`exec_do_given_expr_op` (the `DoGivenExpr` opcode, i.e. `given` in expression
position) drops it in its `Ok` branch:

```rust
Ok(()) => {
    if self.stack.len() > stack_base { last = self.stack.pop().unwrap_or(Value::NIL); }
    self.stack.truncate(stack_base);          // src/vm/vm_given_when_ops.rs:314-320
}
```

but **not** in its succeed branch:

```rust
Err(mut e) if e.is_succeed() => {             // src/vm/vm_given_when_ops.rs:321-332
    self.container_ref_var = ...;
    if let Some(v) = e.return_value { last = v; }
    loan_env!(self, set_when_matched(true));
    // <- missing: self.stack.truncate(stack_base);
}
```

and then unconditionally `self.stack.push(last)` at `:365`. The statement-form
twin `exec_given_op` gets this right (`:224-232` truncates before pushing, and
`:243-249` enforces "always net exactly +1"), which is why
`my $x = given 2 { when 2 { "two" } }` and a bare statement `given` both look
fine.

## Fix

Add `self.stack.truncate(stack_base);` to `exec_do_given_expr_op`'s
`is_succeed` branch, so the branch matches the `Ok` branch's discipline and the
op nets exactly +1 in every path — the invariant `exec_given_op` already
documents for itself.

**Ready for direct implementation.** Pin with a `t/` test covering the three
repro lines above plus the already-correct assignment form
(`my $x = (given 2 { when 2 { "two" } })`) so the fix cannot regress it.

## Relationship to ADR-0052

ADR-0052 Slice 1 generalizes this into a stack-base discipline for every
construct that runs a statement range, and Slice 3 removes the double transport
that makes the mistake possible at all. Landing this ticket first is fine and
expected; Slice 1 then subsumes it.
