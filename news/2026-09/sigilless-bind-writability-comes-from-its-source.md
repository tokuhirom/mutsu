# A sigilless bind's writability comes from its source, not from the slot it lands in

`todo/deep/mainline-lexical-sigilless-binding-leaks-into-a-later-redeclaration.md`
reported that a named sub closing over a sigilless binding leaks that binding
into a later same-named declaration: after

```raku
{ my $s = "a"; my \x := $s; sub named-writer { x = 42 }; named-writer(); }
```

a sibling block's `my \x := 5` was still writable, so `x = 9` silently succeeded
where raku dies with `Cannot modify an immutable Int (5)`.

## The ticket's headline repro was stale — and its diagnosis was wrong

Re-run on 2026-09-04, the three-assertion repro passes verbatim, and it passes
at the very commit the ticket was filed against (`5a30d7418`). So did every
variation probed around it: two blocks each with their own capturing named sub,
plain `my` scalars, an alias to an array element, a capturing block before *and*
after the reader.

What survived was one shape the ticket never isolated: a bind to a **method
result**. Restoring the colliding name `x` to section 5 of
`t/bind-alias-is-a-container.t` (which the ticket had renamed away rather than
covered) failed exactly one assertion — `my \x := $s.uc` stayed writable. The
literal binds it named were already fixed.

And the mechanism was not the one the ticket named. It attributed the leak to
ADR-0024's `unit_lexicals[MAINLINE_UNIT_KEY]` entry outliving its block, and
proposed re-keying that store. That store is not involved: `MarkSigillessBind`
never consulted it, and the ADR-0024 read path is gated on the *running frame*
being the capturing sub.

## What it actually was

`OpCode::MarkSigillessBind` ran **after** the declaration's store and asked
whether the destination slot now held a container. That question is ambiguous,
because a slot becomes a `ContainerRef` for reasons that have nothing to do with
the bind in front of it:

- Same-named `my` lexicals in one compiled unit share ONE slot (the bytecode for
  the repro has a single `x` entry in `code.locals`).
- `CompiledCode::needs_cell_named_sub` records own locals that a directly-nested
  named sub *writes*, so the VM can box them into a shared cell at their
  declaration site (`box_decl_local_cell`) — and it is keyed by **name**,
  because a name is all that is available while same-named locals collapse onto
  one slot. Its own doc comment already warned this "must only fire for locals a
  named sub actually writes — never for an unrelated same-named local in a
  sibling block", and ADR-0032 D2 states the sibling constraint as
  "slot-addressed, never name-addressed".
- So `sub named-writer { x = 42 }` makes *every* declaration through that slot
  box, including the sibling block's. `my \x := $s.uc` stored a plain `Str`, the
  boxing wrapped it in a cell, and the post-store inspection read that cell back
  as evidence of an alias.

A literal bind escaped only by accident: the compiler statically marks
`my \x := 5` readonly and emits no `MarkSigillessBind` at all.

## The fix

The bind SOURCE is the unambiguous oracle, and it is on the stack immediately
before the store. A new `OpCode::MarkSigillessBindSource`, emitted by the
declaration itself, takes the verdict there; `MarkSigillessBind` stays after the
store (a declaration *clears* the name's inherited readonly marker, so the
marker has to be written afterwards) and now consumes that verdict. The pair
carries the name, so a store that re-enters user code cannot make one
declaration consume another's verdict, and the old slot inspection survives only
as the fallback for a `MarkSigillessBind` reached without one.

One subtlety the stack oracle has to respect: a `:=` bind to an index wraps its
result in a `WrapVarRef` over a synthetic `__mutsu_bind_index_ref_N` temp. That
wrapper denotes nothing of its own, so the value under it is the oracle — which
is what keeps `my (\a) := (5,)` immutable while `my \x := @a[0]` writes through.

`needs_cell_named_sub` is still name-keyed. Making it slot-addressed would not
have helped here (both declarations *are* one slot); that residue belongs to
§1.3 of `docs/lexical-scope-slot-campaign.md`, the same root as
`todo/tickets/same-named-loop-params-in-one-unit-interfere.md`.

## Coverage

`t/sigilless-bind-writability-source.t` — 16 assertions, all dual-oracled
against raku: the colliding-name shape in both textual orders, every
container-denoting source (scalar, array element, computed index, hash element,
`is rw` accessor, whole array), the list-destructuring spelling, and a loop that
re-declares the alias each iteration. `t/bind-alias-is-a-container.t` (34) and a
63-file targeted roast sweep of every whitelisted file using `my \` stay green.
