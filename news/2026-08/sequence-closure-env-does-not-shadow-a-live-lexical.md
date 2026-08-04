# A sequence generator's captured env no longer shadows a live lexical

`roast/integration/advent2012-day14.t` aborted after 3 of its 6 assertions under
the real `Test` module with `X::Cannot::Empty` raised inside its own
`is-prime-beta`, and passed under the native provider. The cause was neither in
`Test.rakumod` nor in the test file: `use Test` merely *loaded* a module that
contains `&CALLER::LEXICAL::("infix:<$op>")`, and that is enough to change how
every closure in the process captures its environment.

## What connects the two

`REFLECTIVE_NAME_ACCESS_SEEN` (`src/opcode.rs`) is a process-global, monotonic
flag: once any compiled chunk anywhere contains `EVAL`, a `CALLER::`/`OUTER::`
read, a pseudo-stash lookup or a symbolic deref, `capture_closure_env` stops
capturing a closure's *free variables* and captures the **whole environment by
value** instead — it has to, because reflective code can ask for any name.
`Test.rakumod`'s `cmp-ok` is such a chunk, so importing `Test` degrades every
unrelated closure in the test file to a whole-env snapshot.

That snapshot is harmless for names the closure actually closes over: those are
boxed into shared `ContainerRef` cells, so later mutation is tracked. It is not
harmless for the bulk names dragged along with them, because a sequence
generator's env is **merged over** the live environment before its body runs
(`sequence_closure_step`, and the seed pre-check in `eval_sequence`) rather than
replacing it. Every captured name therefore shadows the caller's current
binding.

A self-referential sequence is exactly where that goes wrong:

```raku
my @primes = 2, 3, 5, -> $p { ($p+2, $p+4 ... &is-prime-beta)[*-1] } ... *;
sub is-prime-beta($n) { $n %% none @primes ...^ * > sqrt $n }
```

The generator is created while `@primes` is still the hoisted empty array. Eager
generation re-enters it, reads the empty `@primes`, and raises — which is
expected and already handled: `deferred_after_generator_error` hands back a lazy
closure sequence precisely so the generator re-runs once `@primes` is bound. But
on that later pull the stale snapshot was re-imposed on the live env, so
`is-prime-beta` still saw an empty `@primes` and raised again, this time with
nowhere to defer to. `@primes` is not a free variable of the generator (the
generator only mentions `&is-prime-beta`), so it was never a genuine capture —
only bulk.

## The fix

Both merge sites now go through `install_sequence_closure_env`. A captured entry
is installed unconditionally when it is a genuine capture — a free variable of
the closure, an `owned_captures` loop binding, or an `authoritative_captures`
vouch — or when it is not a *plain user lexical*
(`crate::env::is_plain_user_lexical`: the system names, dynamics, specials and
shadow-meta a closure body can reach through dedicated opcodes the free-var scan
cannot see). A plain user lexical the closure does not close over is installed
only when the live env has no binding of its own, so an escaped closure keeps
today's behaviour while a live binding is left alone.

This is the same classification the non-reflective capture path already uses to
decide what to drop; the merge simply had no equivalent guard.

`roast/integration/advent2012-day14.t` now passes 6/6 under the real `Test`
module. Pin: `t/sequence-self-reference-under-reflective-capture.t`, which flips
the reflective flag with an uncalled `CALLER::` routine so the shape reproduces
without loading a module.

## Worth carrying forward

**A bug that appears only when a module is loaded is not necessarily about that
module.** Bisecting `Test.rakumod` down to a single routine — by top-level
brace-balanced chunks, since truncating the file at a line number breaks parsing
long before it changes behaviour — showed the trigger was `cmp-ok`, and `cmp-ok`
is never called. Merely *compiling* it set a process-global flag. Any measurement
that assumes the module's routines have to run to matter will miss this class.
