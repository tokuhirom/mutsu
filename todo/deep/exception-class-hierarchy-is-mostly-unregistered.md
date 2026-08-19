# `X::` exception ancestry — design lives in ADR-0029; mechanism and data have landed, a narrow residue remains

**Status (2026-08-19): the design question this ticket was filed to raise is
answered and shipped.** It is recorded in
[ADR-0029](../../docs/adr/0029-exception-class-role-membership.md) — *"Built-in
`X::` exception ancestry is role membership, not a single parent — register it
through the existing composed-role path"* — whose Slices 1, 2 and 3 landed on
2026-08-17/18 (#6590, #6591, #6595). Do **not** re-design this; read ADR-0029
first, then the residue list below.

This ticket stays open only because the residue below is real and unowned. Its
original body (the 2026-08-03 filing, preserved at the end for provenance) is
**stale in every number and in its opening repro** — `X::Bind::Slice.new`,
`X::Numeric::DivideByZero.new` and `X::Attribute::Required.new` all work today.

## What shipped

`src/runtime/runtime_init.rs` now models `X::` ancestry the way rakudo does:

- `register_x` takes `(name, parent, does)` (`runtime_init.rs:1630`). `parent`
  drives `parents`/`mro` as before; `does` is collected into `register_x_does`
  and **never** touches the MRO. There are 367 call sites (up from the ADR's
  107, and the ticket's original 77).
- The role-shaped `X::` namespace segments are seeded as real `RoleDef`s, not
  classes (`runtime_init.rs:2570-2605`) — 16 of them: the ADR's 14 plus
  `X::Nominalizable` and `X::Role::Attribute`, which Slice 3's broader capture
  surfaced.
- Role-to-role composition goes into `registry.role_parents`
  (`runtime_init.rs:2716-2724`), and the collected `does` lists are flattened
  through it and written to `class_composed_roles`,
  `class_direct_composed_roles` and `class_does_only_roles`
  (`runtime_init.rs:2734-2757`) — the same registries the real user-facing
  `does` trait handler writes and that `.^roles`, `.^does`, `~~`, qualified
  dispatch and method-candidate collection already read.
- The hand-written `X::Comp::AdHoc` MRO splice is gone; the class is now
  `parent = "X::AdHoc"`, `does = ["X::Comp"]` (`runtime_init.rs:2282`).
- Data capture is mechanical and checked in:
  `scripts/adr0029-capture-x-exception-data.py` +
  `scripts/probe-x-exception-shape.raku`, emitting
  `TODO_roast/x-exception-role-membership.tsv` (373 real rakudo `Exception`
  subtypes) and `TODO_roast/x-exception-role-membership-diff.tsv`.
- Pins: `t/exception-role-membership.t`.

Re-measured on `main` @ `829745e5c` (2026-08-19, debug build), by re-running the
checked-in capture script — i.e. these numbers are regenerable, not transcribed:

```
$ python3 scripts/adr0029-capture-x-exception-data.py
derived 520 candidate names (never hand-typed)
373 / 520 are real rakudo Exception subtypes
  match: 357      wrong_mro: 0      wrong_roles: 15      missing: 1
```

`wrong_mro: 0` is the headline: the 43 wrong-`.^mro` rows the ADR measured
(36 of them carrying a **false superclass**) are gone, and `.^roles` is no
longer universally empty. `.new` now succeeds for **372 of the 373** names —
the `X::Method::NotFound … new on <class>` signature that defined this ticket
survives on exactly one class.

## Residue — the concrete work still owed (measured 2026-08-19)

This is a narrow gap on top of an already-decided mechanism, so it does **not**
need a new ADR. It is five items, in the order they should be done. Items R1
and R2 together close the last 3 of the 16 diff rows; R3 makes the acceptance
criterion checkable; R4 and R5 are hygiene and the deferred measurement.

### R1. One role-to-role edge is missing: `X::Role::Attribute does X::RoleApplier`

`runtime_init.rs:2716-2724` seeds exactly two edges, with a comment stating
*"exactly two edges exist; the other twelve compose nothing"* — true of the
ADR's **14**-role list, but Slice 3 grew that list to 16
(`runtime_init.rs:2585-2586`) without re-running the edge measurement.
`Registry::builtin_role_parents` (`src/runtime/registry.rs:1170`) has no `X::`
entries either, so nothing else supplies it.

The real edge set, re-derived mechanically over the 16 registered role names:

```
$ raku -e 'for <X::Comp X::Syntax X::IO X::OS X::Trait X::Proc::Async X::BadType
             X::Temporal X::MOP X::Encoding X::Pod X::Wrapper X::RoleApplier
             X::RoleApplier::Method X::Nominalizable X::Role::Attribute> -> $n {
    my $r = ::($n); my @p = try { $r.^roles.map(*.^name) } // ();
    say "$n -> @p[]" if @p }'
X::Syntax          -> X::Comp
X::IO              -> X::OS
X::Role::Attribute -> X::RoleApplier      # <-- not seeded in mutsu
```

Observable consequence — two classes answer `~~` wrongly, which is a *narrowing*
false negative (rakudo says True, mutsu says False):

```
$ mutsu -e 'say X::Role::Attribute::Conflicts.^roles.map(*.^name)'
(X::Role::Attribute)
$ raku  -e 'say X::Role::Attribute::Conflicts.^roles.map(*.^name)'
(X::Role::Attribute X::RoleApplier)
$ mutsu -e 'say X::Role::Attribute::Conflicts.new ~~ X::RoleApplier'
False        # raku: True
```

`X::Role::Attribute::Exists` is the second affected class
(`runtime_init.rs:1970-1978`).

**Fix**: add a third `registry.role_parents.insert("X::Role::Attribute", vec!["X::RoleApplier"])`
alongside the existing two, and — this is the part that stops the same drift
recurring — make the capture script (or a `#[test]`) derive the edge set from
the same registered-role list rather than leaving it as a prose comment. The
comment at `runtime_init.rs:2716-2718` is currently a hand-maintained assertion
about data, which is exactly what ADR-0029's Decision item 5 forbids
everywhere else.

### R2. `X::TooLateForREPR` — the single remaining unconstructible class

It is the one `missing` row, and the only survivor of the signature this ticket
was named for:

```
$ mutsu -e 'X::TooLateForREPR.new'
X::Method::NotFound: … new on X::TooLateForREPR
$ mutsu -e 'say X::TooLateForREPR.^mro.map(*.^name)'
(X::TooLateForREPR Any Mu)          # not even an Exception
$ raku  -e 'say X::TooLateForREPR.^mro.map(*.^name);
            say X::TooLateForREPR.^parents(:local).map(*.^name);
            say X::TooLateForREPR.^roles.map(*.^name)'
(X::TooLateForREPR X::Comp Exception Any Mu)
(X::Comp)
(X::Comp)
```

This is rakudo's **role-as-superclass pun**: `X::Comp` is simultaneously an MRO
entry *and* a composed role for this one class. `ANALYSIS.md:473-474` records it
as a shape "`register_x` cannot express". Measured today that is **not quite
right** — it is expressible as
`register_x("X::TooLateForREPR", "X::Comp", &["X::Comp"])`, because `X::Comp` is
still registered as a `ClassDef` (see R4) so `register_x`'s parent walk
(`runtime_init.rs:1631-1647`) resolves it and produces
`X::TooLateForREPR X::Comp Exception` exactly.

**The catch, and why this needs a deliberate decision rather than a one-line
patch**: it collides head-on with `t/exception-role-membership.t:28` —
*"the 14 role-shaped `X::` marker roles must never leak into a class's MRO"* —
and with ADR-0029 acceptance criterion 1, which states the same as a global
invariant. The invariant is right for 372 classes and wrong for this one,
because rakudo itself violates it here.

**Fix**: land the registration, and amend the pin from a blanket "no role name
in any MRO" to "no role name in any MRO **except where the captured raku data
says otherwise**" — i.e. drive the assertion off
`TODO_roast/x-exception-role-membership.tsv` instead of off a hardcoded rule, so
the exception is data, not a carve-out. Then correct the ADR's criterion 1
wording in the same PR. If the pin cannot be made data-driven cheaply, the
acceptable fallback is an explicitly-named single-class exemption with this
ticket's `raku` output quoted next to it — but not a silent relaxation.

### R3. Acceptance criterion 3 ("the diff is empty") is unreachable as written — 13 rows are a raku duplicate-emission artifact

The other 13 `wrong_roles` rows are all the same shape: rakudo's `.^roles`
emits a role **twice** when it is reached both directly and through a
superclass, and mutsu dedups.

```
X::Attribute::Regex        raku roles: X::Comp,X::Comp            mutsu: X::Comp
X::Comp::Trait::Unknown    raku roles: X::Comp,X::Trait,X::Trait  mutsu: X::Comp,X::Trait
X::Syntax::NonListAssoc…   raku roles: X::Syntax,X::Comp,
                                       X::Syntax,X::Comp          mutsu: X::Syntax,X::Comp
```

In all 13 the composed-role **set** is identical, so every observable that
matters (`~~`, `.^does`, method-candidate collection) already agrees. But
ADR-0029 acceptance criterion 3 demands the script's diff be *empty*, which
these rows prevent, and a permanently non-empty diff is a broken tripwire — it
trains readers to ignore it, which defeats the whole point of checking the
script in.

**Fix**: make the script's *verdict* compare `roles_all` as a set while keeping
the raw rakudo output verbatim in the data TSV (so nothing is lost), and record
the normalisation and its justification in the script header next to the two
existing hard rules. Then criterion 3's "empty diff" becomes true and stays
meaningful. Do **not** instead teach mutsu to replicate rakudo's duplicate
emission — `.^roles` duplicates are a rakudo implementation detail, not a
semantic, and reproducing them would add a private quirk to satisfy a
measurement.

### R4. The 16 marker names are still dual-registered as both `ClassDef` and `RoleDef`

`register_x("X::Comp", "Exception", &[])` and
`register_x("X::Syntax", "X::Comp", &[])` (`runtime_init.rs:1676-1679`) still
create `ClassDef`s for names that the seed block at `runtime_init.rs:2570-2605`
also registers as `RoleDef`s. The block's own comment concedes these are
*"names raku itself does not recognise as real Exception subtypes"*.

Observable divergence:

```
$ mutsu -e 'say X::Comp.^mro.map(*.^name)'   ->  (X::Comp Exception Any Mu)
$ raku  -e 'say X::Comp.^mro.map(*.^name)'   ->  dies: No such method 'mro' for
                                                 Perl6::Metamodel::ParametricRoleGroupHOW
```

mutsu already reports `X::Comp.HOW.^name` as `ParametricRoleGroupHOW`, so the
metaobject answer is right and only the `ClassDef` shadow is wrong. It is not
inert: R2's parent walk depends on it, and `X::Syntax::Signature`
(`runtime_init.rs:1679`) inherits from `X::Syntax` as a class.

**Fix**: decide explicitly, after R1/R2, whether to (a) keep the shadow and
document at `runtime_init.rs:1676` *why* it is load-bearing (it makes the R2
pun expressible and gives `X::Syntax::Signature` a parent), or (b) remove it and
re-route both dependants. (a) is the honest cheap answer and is defensible;
what is not defensible is leaving it undocumented, since the next reader will
correctly read it as a leftover of the pre-ADR-0029 mis-modelling and delete it.
This is the only residue item with no user-visible bug attached — it is a
comment-and-decide task, not a fix.

### R5. ADR-0029 Slice 4 (the honest measurement) is still deferred, and this ticket's own probe has expired

Slice 4 is *"re-run the real-`Test` sweep and report the delta as measured,
including if it is small"*. It is blocked on the separate, still-open
[`todo/deep/vendor-real-test-module.md`](vendor-real-test-module.md), and that
block is the sole reason ADR-0029's Status is not yet `Accepted`.

One correction to carry into Slice 4: **the role-only probe this ticket
designated no longer measures anything.** `roast/S02-literals/quoting-unicode.t`
was the ticket's chosen clean isolator for the role half — it is now whitelisted
(`roast-whitelist.txt:85`) and passes 101/101 under mutsu's native `Test`:

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

So Slice 4's remaining value is **exclusively** the vendored-real-`Test` sweep,
not this file. Whoever runs it should say so, and should still expect a small or
null delta — that expectation is ADR-0029's, stated up front, and a small number
is the correct outcome, not a disappointment.

## Closing this ticket

When R1-R4 land and Slice 4 is either run or formally re-scoped:
`git mv` this file to `news/YYYY-MM/` and rewrite it as an accomplishment
(`todo/README.md`), give ADR-0029 an `Outcome` section, and move its Status to
`Accepted`. Until then the file stays here so the residue is not lost — the
correctness work is done, but "done" is not the same as "closed".

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
