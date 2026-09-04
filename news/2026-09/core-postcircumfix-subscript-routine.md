# The subscript operators are real CORE routines now, and the delegation idiom no longer overflows the stack

In Raku the bracket and brace subscripts are ordinary (multi) subs in CORE, not
just syntax. That matters for a module that wants to add its own subscript
candidates for its own type and hand every other shape back to the built-in
behaviour: it captures the core routine as a term first, then delegates to it.
`Array::Rounded` is the canonical example.

```raku
class Rounded is Array {}

my constant &old-same = &postcircumfix:<[ ]>;

proto sub postcircumfix:<[ ]>($, |) {*}
multi sub postcircumfix:<[ ]>(Rounded:D \SELF, Int:D $index) { old-same SELF, $index }
multi sub postcircumfix:<[ ]>(Rounded:D \SELF, Any:D \index)  { old-same SELF, index.round }

my Rounded $r = Rounded.new(10, 20, 30, 40);
say $r[1.6];    # raku: 30
```

mutsu ended that program with `thread 'mutsu-main' has overflowed its stack`
and a core dump. It was ranked Tier S in `todo/TRIAGE.md` as a hard crash.

## What was actually wrong

`resolve_code_var` (`src/runtime/accessors_resolve.rs`) has long had a fast
path that turns an operator term into a by-name routine reference, precisely so
that the reference denotes the *operator* and the call path
(`vm_dispatch_helpers.rs`) can give the CORE routine priority over a same-named
user declaration. That is why the identical idiom for an infix has always
worked — `my constant &oldadd = &infix:<+>` above a user `multi sub infix:<+>`
captures core `+`, and calling it adds.

The fast path listed `infix:<`, `prefix:<` and `postfix:<`, but not
`postcircumfix:<`. So `&postcircumfix:<[ ]>` dropped through to the generic
branch, which materialises the *user's own* candidates by value — making
`old-same` the very candidate that called it, forever. The hoist pass is what
made those candidates visible so early, and ADR-0041 diagnosed the crash as a
hoisting problem on that basis; but hoisting is not what made the captured term
wrong. A textually-preceding user candidate would have been captured there too,
and would have recursed the same way.

Adding `postcircumfix:<` to that list is the fix, and it makes the four
subscript-shaped operator terms behave alike rather than adding a special case.

## The routine had to exist first

`&postcircumfix:<[ ]>` previously answered `Nil`, and
`postcircumfix:<[ ]>(@a, 1)` answered "Unknown function": mutsu compiles
`@a[...]` straight to the `Index` opcode family, so the operator existed only
as syntax and there was nothing for a term to denote. Both subscript operators
are now ordinary builtins (`src/runtime/builtins_postcircumfix.rs`). They drive
the same opcode the syntax lowers to, so slices, `Whatever` indices and the
three-argument store form all behave exactly as the syntax does, and the
user-candidate probe in `exec_index_op_with_positional` is suppressed for
exactly that one dispatch — the CORE candidate performs native indexing and must
never re-enter the override that is delegating to it. The suppression is a
one-shot taken at the top of the op, so it can never leak onto a nested
subscript evaluated underneath.

Pinned in `t/core-postcircumfix-routine.t` (the routine itself: by-name calls,
slices, the store form, the captured term) and
`t/user-postcircumfix-core-delegation.t` (the delegation idiom, including
`Int:D` still beating `Any:D` for an integer index).

## What this also settled about ADR-0041

Re-measuring the ADR's premises turned up two that were wrong, both now
recorded in the ADR itself (§6):

- §1.2 claims plain named subs are the scope-blind case and operator names are
  exempt. It is the reverse: a plain `sub` shadows correctly in an inner block,
  while `proto`/`multi` are not lexically scoped at all — an inner `proto`
  raises a false `Redeclaration of routine`, and an inner `multi` merges into
  the outer candidate set instead of shadowing it.
- §4's recommendation (Option B: emit each `RegisterDecl` at its own textual
  position) is unsafe as specified. The discriminator in real Raku is compile
  time versus run time, not textual position at run time: raku installs a sub's
  pad entry at compile time, so an ordinary runtime reference sees the whole
  scope regardless of order. Option B would break `my $old = &foo;` above a
  later `sub foo` — which raku resolves to the later one — and would break a
  call placed before a `constant` that precedes the sub's declaration.

The residue of ADR-0041 — a `&name` reference inside a `constant` initializer or
`BEGIN` block still seeing declarations only the hoist pass has made visible —
stays open, and is now documented as needing the same lexically-scoped sub
registry that the `proto`/`multi` shadowing bug needs.
