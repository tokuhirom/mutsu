# `X::` exception ancestry — design lives in ADR-0029; mechanism, data, and residue have all landed; only Slice 4's measurement remains, blocked elsewhere

**Status (2026-08-19): the design question this ticket was filed to raise is
answered and shipped, and the residue this ticket tracked after that is now
fixed too.** It is recorded in
[ADR-0029](../../docs/adr/0029-exception-class-role-membership.md) — *"Built-in
`X::` exception ancestry is role membership, not a single parent — register it
through the existing composed-role path"* — whose Slices 1, 2 and 3 landed on
2026-08-17/18 (#6590, #6591, #6595), and whose residue items R1-R4 (see below)
landed on 2026-08-19. Do **not** re-design this; read ADR-0029 first.

This ticket now stays open for exactly one remaining item: **R5**, which is
not fixable here because it is blocked on a separate, unrelated, still-open
ticket. Its original body (the 2026-08-03 filing, preserved at the end for
provenance) is **stale in every number and in its opening repro** —
`X::Bind::Slice.new`, `X::Numeric::DivideByZero.new` and
`X::Attribute::Required.new` all work today.

## What shipped

`src/runtime/runtime_init.rs` now models `X::` ancestry the way rakudo does:

- `register_x` takes `(name, parent, does)` (`runtime_init.rs:1630`). `parent`
  drives `parents`/`mro` as before; `does` is collected into `register_x_does`
  and **never** touches the MRO.
- The role-shaped `X::` namespace segments are seeded as real `RoleDef`s, not
  classes (`runtime_init.rs:2570-2605`) — 16 of them: the ADR's original 14
  plus `X::Nominalizable` and `X::Role::Attribute`, which Slice 3's broader
  capture surfaced.
- Role-to-role composition goes into `registry.role_parents`
  (`runtime_init.rs:2716-2730`) — **three** edges (`X::Syntax does X::Comp`,
  `X::IO does X::OS`, `X::Role::Attribute does X::RoleApplier` — the third
  added 2026-08-19, residue R1), and the collected `does` lists are flattened
  through it and written to `class_composed_roles`,
  `class_direct_composed_roles` and `class_does_only_roles` — the same
  registries the real user-facing `does` trait handler writes and that
  `.^roles`, `.^does`, `~~`, qualified dispatch and method-candidate
  collection already read.
- The hand-written `X::Comp::AdHoc` MRO splice is gone; the class is now
  `parent = "X::AdHoc"`, `does = ["X::Comp"]`.
- `X::TooLateForREPR` (rakudo's one role-as-superclass pun in this
  vocabulary — `X::Comp` is simultaneously a real MRO entry *and* a composed
  role) is registered as `parent = "X::Comp"`, `does = ["X::Comp"]`
  (2026-08-19, residue R2) — it was the sole remaining unconstructible `X::`
  class.
- The capture script's mutsu-vs-raku verdict compares the composed-role *set*,
  not the raw string, because real rakudo's `.^roles` emits a role twice when
  it is reached both directly and through a superclass while mutsu correctly
  dedups (2026-08-19, residue R3). The raw rakudo string is kept verbatim in
  the data TSV; mutsu was **not** taught to replicate the duplicate emission.
- The dual `ClassDef`/`RoleDef` registration of `X::Comp` and `X::Syntax` (the
  only two names in this position — confirmed by an exhaustive scan of the
  registry, not an assumption) is documented in place as load-bearing for
  `X::TooLateForREPR`'s parent walk (2026-08-19, residue R4), rather than left
  as an unexplained leftover. Investigating it surfaced one further genuine
  bug: `dispatch_classhow_roles`'s `is_role` gate
  (`src/runtime/methods_classhow_parents.rs`) required
  `!classes.contains_key(name)`, so `.^roles` called directly on a
  dual-registered marker-role name (e.g. `X::Syntax.^roles`) fell through to
  class semantics and wrongly returned `()` instead of `(X::Comp)`. Fixed by
  giving role identity the same priority `.HOW.^name` already gives it.
- Data capture is mechanical and checked in:
  `scripts/adr0029-capture-x-exception-data.py` +
  `scripts/probe-x-exception-shape.raku`, emitting
  `TODO_roast/x-exception-role-membership.tsv` (373 real rakudo `Exception`
  subtypes) and `TODO_roast/x-exception-role-membership-diff.tsv`.
- Pins: `t/exception-role-membership.t` (27 assertions, each cross-checked
  against real `raku` first).

Re-measured on `main` @ `c0a041b21` plus this ticket's own residue fix
(2026-08-19, debug build), by re-running the checked-in capture script — i.e.
these numbers are regenerable, not transcribed:

```
$ python3 scripts/adr0029-capture-x-exception-data.py
derived 520 candidate names (never hand-typed)
373 / 520 are real rakudo Exception subtypes
  match: 373      wrong_mro: 0      wrong_roles: 0      missing: 0
```

Every one of the 373 real rakudo `Exception` subtypes mutsu raises or tests
against now matches raku's `.^mro` and `.^roles` exactly (set-compared, per
R3), and `.new` succeeds for all 373. The `X::Method::NotFound … new on
<class>` signature that defined this ticket no longer reproduces anywhere in
the corpus.

## What remains: R5 — Slice 4's real-`Test` sweep (blocked elsewhere, not a design gap)

Slice 4 is *"re-run the real-`Test` sweep and report the delta as measured,
including if it is small."* It is blocked on the separate, still-open
[`todo/deep/vendor-real-test-module.md`](vendor-real-test-module.md), and that
block is the sole reason ADR-0029's Status section still lists an open item.

Slice 4's originally-designated *role-only* probe has already expired and is
no longer useful for isolating this: `roast/S02-literals/quoting-unicode.t`
was the ticket's chosen clean isolator for the role half — it is now
whitelisted (`roast-whitelist.txt:85`) and passes 101/101 under mutsu's native
`Test`:

```
$ MUTSU_FUDGE=1 prove -e 'target/debug/mutsu' roast/S02-literals/quoting-unicode.t
roast/S02-literals/quoting-unicode.t .. ok        All tests successful.
```

and the specific membership it hinged on now answers correctly for both an
instance and a bare type object:

```
$ mutsu -e 'say X::Comp::FailGoal ~~ X::Comp; say X::Comp::FailGoal.new ~~ X::Comp'
True
True
```

So Slice 4's remaining value is **exclusively** the vendored-real-`Test`
sweep, not this file, and not this ADR's mechanism. Whoever runs it should say
so, and should still expect a small or null delta — that expectation is
ADR-0029's, stated up front, and a small number is the correct outcome, not a
disappointment. **This is not a task for this ticket to do directly** — it is
gated on `vendor-real-test-module.md` landing first, and belongs there or as
its own follow-up once that lands.

## Closing this ticket

This file can be `git mv`-ed to `news/YYYY-MM/` and rewritten as an
accomplishment once `todo/deep/vendor-real-test-module.md` lands and Slice 4's
sweep is run and its result recorded in ADR-0029's Outcome section — at that
point ADR-0029 needs no further update from this ticket either. Until then the
file stays here so R5 is not lost, even though it is understood to be
someone else's prerequisite, not open design work.

---

## Appendix: the original 2026-08-03 filing (historical — every number below is superseded)

Retained for provenance. Its central warning is what ADR-0029 was written to
honour and is still the right instinct; its measurements are not current.

> mutsu registers 77 `X::` classes in `src/runtime/runtime_init.rs`. It *raises*
> far more than that: 124 core exception classes appear in mutsu's own source
> but are not real types, so `.new` on them does not exist.
>
> ```
> $ mutsu -e 'say X::Bind::Slice.new.^name'
> X::Method::NotFound: Unknown method value dispatch (fallback disabled): new on X::Bind::Slice
> $ raku  -e 'say X::Bind::Slice.new.^name'
> X::Bind::Slice
> ```
>
> **Why this needs design, not a mechanical sweep.** The obvious rule — parent =
> the longest `::`-prefix that is itself registered — is wrong more often than
> right, so applying it would bake false inheritance into the type system:
> `X::IO::Mkdir`/`X::IO` and `X::Syntax::Perl5Var`/`X::Syntax` hold, but
> `X::Bind::Slice`/`X::Bind`, `X::Numeric::DivideByZero`/`X::Numeric`,
> `X::Parameter::RW`/`X::Parameter`, `X::Attribute::Required`/`X::Attribute`,
> `X::Placeholder::Block`/`X::Placeholder`,
> `X::Syntax::Malformed::Elsif`/`X::Syntax::Malformed` and
> `X::Str::Sprintf::Directives::BadType`/`X::Str::Sprintf::Directives` do not.
> The reason is structural: in rakudo the shared behaviour is carried by
> **roles** (`X::Comp`, `X::Syntax`, `X::OS`, …), not by superclasses.
>
> A worked example of why measuring the payoff comes first: three files in the
> Test-vendoring sweep were filed here because their failure text named two
> `X::Undeclared*` classes, and two of them were not hierarchy problems at all —
> mutsu raised the *wrong class*
> (`news/2026-08/undeclared-variable-is-not-undeclared-symbols.md`). Registering
> `X::Undeclared::Symbols` under `X::Undeclared` to "fix" them would have baked
> in inheritance raku does not have. **Read the failure, do not pattern-match
> the name** — this rule survives verbatim into ADR-0029's final Risk.
