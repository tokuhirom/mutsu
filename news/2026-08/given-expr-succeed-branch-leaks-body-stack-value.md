# An expression-position `given` with a matching `when` no longer leaks a stack value

Found while verifying `todo/deep/when-nonmatch-value-outside-map-grep.md`
against `main` @ `4c58b5f59` (2026-08-20), filed as a small independent
ticket, and fixed the same day.

## Repro

```raku
say "A: ", (given 2 { when 2 { "two" } });   # raku: "A: two"   mutsu (before): "twotwo"
say "D: ", (given 3 { default { "d" } });    # raku: "D: d"     mutsu (before): "dd"
say "P: ", (given 1 { when 99 { "a" }; when 1 { "b" } });  # raku: "P: b"  mutsu (before): "bb"
```

The literal `"A: "` was silently dropped: the `given` expression pushed
**two** stack values instead of one, so `Say(2)` consumed both of them and
left the label stranded below. Any call whose argument list contained an
expression-position `given` with a matching `when`/`default` was affected,
not just `say`. A non-matching `when` was unaffected (it pushes nothing —
that is the [ADR-0052](../../docs/adr/0052-a-when-clause-produces-its-value-on-the-stack.md)
finding).

## Root cause

`exec_when_op` (`src/vm/vm_given_when_ops.rs`) does not *pop* the matched
body's value off the stack; it peeks it into the `succeed` signal's
`return_value`. The value therefore exists twice, and each enclosing
construct is responsible for dropping the stack copy.

`exec_do_given_expr_op` (the `DoGivenExpr` opcode, i.e. `given` in
expression position) dropped it in its `Ok` branch but not in its
`is_succeed` branch, then unconditionally pushed `last` at the end. The
statement-form twin `exec_given_op` already got this right (truncates
before pushing, and enforces "always net exactly +1"), which is why
`my $x = given 2 { when 2 { "two" } }` and a bare statement `given` both
looked fine even before this fix.

## Fix

Added `self.stack.truncate(stack_base);` to `exec_do_given_expr_op`'s
`is_succeed` branch, matching the `Ok` branch's discipline so the op nets
exactly +1 in every path -- the invariant `exec_given_op` already documents
for itself. Pinned with `t/given-expr-succeed-no-double-push.t`, covering
the three repro lines above, the already-correct assignment form, and an
explicit `succeed EXPR` inside a `when` in expression position.

## Relationship to ADR-0052

ADR-0052 Slice 1 generalizes this into a stack-base discipline for every
construct that runs a statement range, and Slice 3 removes the double
transport that made the mistake possible at all. Landing this fix first was
fine and expected; Slice 1 subsumes it going forward.
