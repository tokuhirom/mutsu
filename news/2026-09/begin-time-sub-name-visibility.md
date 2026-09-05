# A BEGIN-time name reference now sees only the sub declarations the program has reached

mutsu registers every top-level `sub` of a block through a hoist pre-pass
emitted at the head of that block, so a name is callable from anywhere in its
enclosing scope regardless of textual order. Rakudo gets the same effect a
different way: it installs the routine's pad entry at *compile* time. The two
models agree for an ordinary runtime reference — which is order-blind in both —
but they diverge for a reference evaluated at **BEGIN** time, inside a
`BEGIN`/`CHECK` body or a `constant` initializer, because rakudo's compilation
has only reached part of the scope at that moment while mutsu's hoist had
already registered all of it.

ADR-0041 §6.3 tabulated the divergence and §8 recorded a failed attempt at it.
Both rows are now fixed:

```raku
sub foo() { "outer" }
{
    my constant &old = &foo;   # rakudo: the OUTER foo; mutsu used to capture the inner one
    say old();
    sub foo() { "inner" }
}

constant X = f();              # rakudo refuses to compile; mutsu used to print 42
sub f() { 42 }
say X;
```

The blocker was not the rollback itself but the question it depends on: *has
this declaration been reached yet?* §8 tried to derive that from registry
writes and correctly found it undecidable — an in-sequence registration whose
hoisted twin is byte-identical takes the idempotent
`SubRegisterOutcome::Unchanged` path and writes nothing at all. Its second
finding, that a plain mainline `sub` leaves no `registry().functions` entry to
record a displacement against, does not reproduce: a `rust-gdb` breakpoint on
the single-candidate install shows it firing with `fq` = `GLOBAL::foo`, exactly
where a displacement record needs it.

The signal that does work needs no source positions and no registry write at
all. `exec_register_sub_op` already knows, before it registers anything, whether
it is running the hoist copy of a declaration (the `__hoisted` marker the
pre-pass stamps on) or the in-sequence one, and that is true on every execution
whatever the registration outcome turns out to be. So a hoist-pass registration
records what it displaced, the in-sequence registration drops that record, and
a BEGIN-time region rolls every still-recorded declaration back to what it
displaced — which is precisely the routine rakudo would have found. Restoring
the displaced def, rather than merely hiding the unreached one, is what makes
the block-nested case answer `outer` instead of failing to resolve; ADR-0041
§6.4 had flagged that as the reason a simple suppression could not work.

The mechanism lives in `src/runtime/hoist_visibility.rs`. It guards itself with
an `Arc::ptr_eq` staleness check, so a record left behind by an early return or
a scope restore is skipped rather than trusted, and it also covers the
value-position `BEGIN` (`OpCode::BeginOnceExpr`), which never raised
`check_phaser_depth` and so was not a BEGIN-time region at all before — making
`my $x = BEGIN foo()` and `BEGIN say foo()` agree.

`t/begin-time-sub-visibility.t` pins all of it, including the two rows that are
regression controls rather than fixes: a plain runtime `&name` capture still
sees a block-local routine declared after it, and a plain forward call still
resolves across a `constant` declaration. Those are the rows ADR-0041 rejected
the "register each declaration at its textual position" design over, and they
must stay green through any future rework of this area.
