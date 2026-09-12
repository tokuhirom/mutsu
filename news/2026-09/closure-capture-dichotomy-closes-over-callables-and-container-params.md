# The closure-capture dichotomy closes over callables and container parameters

`Algorithm::LCS` 0.1.1 died with a stack overflow before its first assertion.
The root cause was not recursion in the distribution: it was two holes in
ADR-0055's capture dichotomy, and closing them took the suite from 0 to 17 of
17 — parity with rakudo.

## The dichotomy, and what fell out of it

ADR-0055 §7.3 states that an escaping-captured lexical is **either**
authoritative — the creating frame vouches for it, so the capture installs with
overwrite semantics — **or** a shared cell. The two halves are meant to be
exhaustive, because a capture with neither defence is indistinguishable from a
same-named lexical in whatever frame happens to be calling the closure, and the
closure-call merge's don't-overwrite default then lets the caller's binding win.
The free variable stops being lexical and starts being dynamic.

A capture that is *mutated* is refused the vouch, and rightly so: a by-value
install would go stale. That makes the cell mandatory for it. Two families were
refused the cell as well, and so had no defence at all.

**Callables.** `ValueView::Sub` sat in the value-kind refusal list in
`box_captured_lexicals`, `&`-sigil names were skipped at the same site, and `&`
was excluded from `needs_cell_unvouched_locals` too. So a reassigned callable —
either `my &f` or a `$f` holding a `Sub` — captured by an escaping closure
resolved by name up the live frame chain. ADR-0025's note for that refusal reads
"`Sub`: the `&` lane has its own registries. Keep.", and `opcode.rs` restated it
as "rebinding `&f` is a name-write the vouch already sees". The second half is
exactly backwards: the vouch *does* see the name-write, and therefore refuses,
leaving nothing behind it. `Sub` was the last unargued entry in that list —
`Package`, `Array` and `Hash` had already left it under ADR-0055 slice 1 on the
same reasoning.

In `Algorithm::LCS` this was self-referential, which is why it crashed rather
than merely answering wrongly. `lcs` assigns its `:&compare-i is copy`
parameter and hands closures over it to `strip-prefix`, whose own parameter is
also named `&compare-i`. The closure resolved to *itself* and recursed until the
stack ran out.

**Container parameters.** The `@`/`%` half of the complement,
`needs_cell_unvouched_containers`, is delivered at the *declaration* site —
ADR-0039 measured per-capture boxing as too expensive, taking
`roast/S15-nfg/concat-stable.t` from 2s to 31s. A parameter has no declaration
store, so that delivery site never ran for one. An `@` parameter captured by an
escaping closure, and handed to a call (so unvouchable, since an `is rw` param
could write it back), reached neither half.

That was the second `Algorithm::LCS` failure, worth 6 of the 17 assertions once
the crash was gone: `lcs`'s default comparator closes over `lcs`'s `@a`/`@b`
parameters but runs inside `strip-prefix`, whose same-named parameters hold the
*reversed* arrays. The comparator indexed the wrong sequences — and past their
ends, where two `Nil`s compare equal, so it reported spurious matches.

## The fix

`&` joins `needs_cell_unvouched_locals`, and the `Sub` value-kind refusal is
lifted for that trigger only. Because the `&` lane resolves names to callables
in its own way, four chokepoints now read through the cell rather than handing
it on: `resolve_code_var`, `lexical_amp_var_callable`, `exec_get_code_var_op`,
and the local-slot fallback in `exec_call_on_code_var_op`. ADR-0067's rw-arg
machinery already assumed a resolved callee could be a `ContainerRef`, so this
follows an existing precedent rather than inventing one.

A container *parameter* takes its cell from `box_captured_lexicals`, scoped to
`code.param_local_slots`. An ordinary `my @a` still takes the declaration site,
so ADR-0039's measured cost is unchanged — only the case that had no site at all
is new.

Pinned by `t/routines/closure/closure-capture-sub-valued-cell.t` and
`t/routines/closure/closure-capture-container-arg-cell.t`, which cover both
sigils, the self-referential shape, a block-level shadower as well as a
parameter, and the mutation-visibility property that makes the cell mandatory
rather than a snapshot.

## Ledger

`Algorithm::LCS` 0.1.1 goes `red` (0 of 17 assertions, `t/01-basic.rakutest`
dying) to `green` (17 of 17, matching rakudo's baseline exactly).
