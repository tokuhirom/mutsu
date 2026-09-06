# A captured parameter gets the cell too — the Cro leak was the name-keyed lane, not the cell

ADR-0055's invariant is that **every escaping-captured plain scalar is either
authoritative or a shared `ContainerRef` cell**. When
`needs_cell_unvouched_locals` shipped on 2026-09-06 it had one hole punched in
it: the frame's own PARAMETERS were filtered out, because boxing one leaked
state between two invocations of a routine and dropped six whitelisted
Cro::HTTP suites in the batteries gate. That hole was recorded as
`todo/tickets/parameter-capture-handed-to-a-call-has-neither-defence.md`. It is
now closed, and the exclusion is gone: a captured parameter gets the cell like
any other own local.

## What the ticket got right, and what it got wrong

Right: the repro, and the fact that both of its lines are load-bearing.

Wrong: its statement of the *conditions*. The ticket said that dropping the
caller's decoy closure (`my $g = { $p }`) makes mutsu answer `OUTER`, i.e. that
only the env-resident collision diverged. Re-measured, the slot-resident form
does not answer `OUTER` — it answers `(Any)`. The closure's binding does not
lose to the caller's value; it vanishes. So the shape was strictly worse than
recorded:

```raku
sub noop($v) { 1 }
sub outer($p) { noop($p); my $f = { $p }; return $f }
my $f = outer("OUTER");
sub caller-frame() { my $p = "CALLER"; $f.() }   # merely DECLARED, never read
say caller-frame();      # raku: OUTER   mutsu was: (Any)
```

Also wrong: its proposed fix ("a parameter binding should reset a stale cell the
way a vardecl does"). There is no stale cell to reset. Every call path installs a
fresh `locals` vector from the pool, so a cell boxed into a parameter's slot
belongs to that invocation alone, and the env mirror of it is a callee-local name
that both return-merge arms drop. Removing the exclusion outright keeps all 3713
`t/` files and 911 whitelisted roast files green, and six recursion / loop /
method / `is copy` shapes built specifically to catch a shared binding all
behave exactly as raku does.

## What the leak actually was

Removing the exclusion did reproduce the Cro regression, deterministically —
which made it reducible. Under
`Cro::HTTP::Client.get` in a four-request loop, the *test script's own*
`my $url = 'http://localhost:31399'` was overwritten with the client's `$url`
parameter after the second request, so the third asked for `/b/a` and the fourth
for `/b/a/b`, both 404. The accumulating path in the original failure message was
the caller's base URL growing, not a client-internal target.

`box_captured_lexicals` publishes a new cell twice. The first publication is into
`env`, under the frame's own local name — correct and per-invocation. The second
is into `shared_vars`:

```rust
if self.shared_vars_active {
    self.thread_redeclared_vars.borrow_mut().remove(&s);
    ...
    loan_env!(self, set_shared_var(&s, container.clone()));
}
```

`shared_vars` is keyed by **bare name** and is process-wide within a lineage. It
exists so a parent thread and a worker see one another's writes to a *declared*
lexical. Publishing a cell there therefore declares "this cell is what this name
means", everywhere — and it explicitly clears `thread_redeclared_vars`, the mask
whose whole job is to say "a different binding now owns this name". A parameter
is the one kind of local for which that claim is never true: it is a binding the
caller creates fresh on every invocation, and its name collides with unrelated
lexicals all over the program. `Cro::HTTP::Client.request`'s `$url` thereby
became the meaning of `$url` in the script that called it.

So the fix is one condition, at the lane rather than at the cell:

```rust
if self.shared_vars_active && !code.param_locals.contains(sym) {
```

`CompiledCode::param_locals` survives, with a new consumer and a narrower claim.
Declining the lane costs a parameter nothing: the lane's purpose is to replace a
*stale plain snapshot* seeded by an earlier `start`, and a parameter cannot have
been snapshotted before it existed. (`set_shared_var` only updates entries that
already exist, so the whole block was a no-op for names the lane never held.)

## Ingredients of the leak, all three load-bearing

A thread must be running (the lane is otherwise inactive); the spawned block must
**not** mention the caller's lexical (a scalar the block captures is excluded from
the lane's seeding, so the collision needs an *uncaptured* seeded name — this is
why every hand-built probe that had the worker read `$url` came back clean); and
the callee's parameter must be both handed to a call (so the frame refuses to
vouch for it) and captured by an escaping closure (so it is boxed at all).

## Pins

- `t/captured-param-cell-not-in-shared-vars-lane.t` — six assertions on the leak
  itself, including the two directions that must NOT change: a *declared*
  lexical keeps the lane (a worker still observes the parent's later writes), and
  a parameter captured by a spawned block still reads its own invocation's value.
  It fails 2 of 6 with the lane condition removed, so it discriminates.
- `t/closure-capture-cell-dichotomy.t` grew 17 → 22: the ticket's repro in both
  residencies (18, 19), and parameter freshness through direct recursion, a
  recursive method with a reassigned `is copy` parameter, and repeated invocation
  from a loop (20-22).
- Batteries gate back at `GATE PASSED: 289/312`.
