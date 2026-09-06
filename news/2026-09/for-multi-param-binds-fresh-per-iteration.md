# A `for` loop's multi-parameter bind is fresh per iteration

A `for` loop's **multi**-parameter list binds through a plain assignment. When a
parameter's name collided with any local slot of the same compiled unit, every
closure created in the body ended up sharing one container, so they all observed
the last iteration:

```raku
my $x = 1;                      # an enclosing `my`, or a popped sibling block's
my @a = 10, 20, 30, 40;
my @c;
for @a -> $x, $y { @c.push(-> { $x }) }
say @c>>.();     # raku: [10 30]   mutsu: [30 30]
```

Silent: exit 0, the enclosing `$x` still read `1` afterwards, only the closures
were wrong.

## The cell the sever could not see

`build_for_bind_stmts` binds a multi-param via a plain `Stmt::Assign`, and
`SetLocal` writes **through** a `ContainerRef` when the slot holds one. The VM
already knew this and already severed such a cell — the `@`/`%` parameters keep
their aliasing, a scalar's binding is cut so the loop-duration value is fresh.
It severed **once, at loop entry**.

That covers a cell that existed *before* the loop, which is the one the previous
investigation traced: the bind's own `SetLocal` makes `compute_free_vars` record
the name as mutated, the outer local is classed `captured_mutated_locals`, and
`box_captured_lexicals` boxes it. Both candidate fixes recorded in the ticket
attacked that chain — declare instead of assign, or exclude the bind ops from
the mutation scan by position — and both fell short, the second because a
genuinely-mutated outer lexical re-earns its cell however the scan is filtered.

But a cell is also minted **during** the loop. A closure created in the body
captures the parameter's local by boxing that very slot, so iteration 1's
closure leaves a cell in the slot that iteration 2's bind then writes through —
and the closure iteration 1 handed out reads iteration 2's value. Loop entry is
too early to see it.

So the sever runs before every iteration instead of once. That is all it takes:
whichever of the two minted the cell, this iteration's binding is cut from it,
and the bind installs a fresh value. The cell itself is left alone — the
existing `saved_multi_params` snapshot still holds it for the post-loop restore,
and anything that legitimately captured it keeps reading it, which is why the
residue that defeated fix (B) is simply not a residue here: an outer lexical
that was genuinely written elsewhere still reports that write through its own
cell.

The two call sites share one `sever_multi_param_cells`, and the list of
severable names (`@`/`%`/`&` excluded) is computed once for the loop rather than
per iteration.

## Not the campaign it looked like

The ticket placed this in §1.3 of `docs/lexical-scope-slot-campaign.md`
(slot-indexed locals / retiring the by-name resolvers), reasoning that two `$x`
bindings in one unit are one slot, so no per-binding cell decision is
expressible. That reasoning is about deciding *whether a slot gets a cell*.
Making the binding fresh does not need that decision — it only needs the binding
cut at the right moment, and the right moment is every iteration. §1.3 is
unaffected and still worth doing on its own terms; `ForLoopSpec`'s
`multi_param_locals` snapshot and the `saved_multi_params` apparatus are
untouched, so fix (A)'s obstacle (re-keying that snapshot off something other
than a pre-bind `local_map`) never had to be solved.

Pinned by `t/for-multi-param-fresh-per-iteration.t`: 12 assertions covering the
headline, both parameters, the popped-sibling collision, the no-collision case
that always worked, the genuine-outer-mutation residue, nested same-named loops,
the single-parameter form, and the `@`-parameter aliasing that must survive.
All measured against rakudo 2026.07 and passing identically under both.
