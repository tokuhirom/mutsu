# `nextcallee` keeps its deferral chain; `DYNAMIC::<$*x>` reaches light calls

The callable a method's `nextcallee` returns is now a continuation that carries
the rest of the method's deferral chain, as rakudo's dispatcher continuation
does. Red's `ScopedDriver` calls it from a closure passed to another method
(`$!guard.protect: { next(self, |c) }`); the candidate it reached then ran with
whichever deferral frame was innermost, so its `callsame` returned `Nil` or
followed the wrong chain. The continuation is one compiled template copied with
the chain in its env, and each later candidate is a continuation of its own.

`DYNAMIC::<$*x>` now finds a caller's `my $*x` from a routine called with
arguments (a light call that pushes no call frame) and from a closure `map`
runs in a nested VM run, where it read `Nil` while `$*x` itself worked. Red's
t/101 reads its query-plan-cache flag that way in a `TWEAK`.

A hyper method call over an array that contains itself (`@a.push: @a;
@a>>.WHICH`) now dies with the catchable deep-recursion error instead of
overflowing the native stack.

A routine or `do` block that holds a `CATCH` now returns a Failure its tail
statement yields; only a `try` throws it into the handler, as in rakudo (Red's
`automatic-transaction`, t/097).

The declaration a call of a unit's sub resolves to answers its `body` from the
unit's tree, so Red's compiler can inline a helper sub (t/124).

`nqp::unipropcode('Block')` and the Block value codes and names work, with the
block table brought to Unicode 17 as MoarVM has it (Red t/121).

`Block.placeholder-signature`, `Code.static_id` and `Routine.is-wrapped` are
implemented; `static_id` is the same for the code a CHECK phase saw and the
code the restarted unit runs, so Red's compiled-callable guard accepts it
(t/124).
