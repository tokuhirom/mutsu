# ADR-0104: `$*RAKU.compiler.version` is a Rakudo-release coordinate, not mutsu's package version

- Status: Accepted (implemented)
- Date: 2026-09-15
- Issue: [#8403](https://github.com/tokuhirom/mutsu/issues/8403)

## Context

`Proc::Q` 1.001003 cannot be loaded under mutsu at all. Its first statement is

```raku
use RakudoPrereq v2017.05.347.g.61.ecfd.5,
  'Proc::Q module requires Rakudo v2017.06 or newer';
```

and `RakudoPrereq`'s `EXPORT` routine is, in full:

```raku
my $out = ($*PERL.compiler.name ne 'rakudo' and %opts<rakudo-only>)
    ?? ($user-message || $message)
    !! ($*PERL.compiler.version before $v)
        ?? ($user-message || "$message version $v.perl() or newer; this is"
          ~ " $*PERL.compiler.version.perl()")
        !! return Map.new;
note $out;
exit 1;
```

mutsu reported `$*PERL.compiler.version` as its own crate version, `v0.23.0`.
`v0.23.0 before v2017.05…` is `True`, so the guard fires and the process exits
before `Proc::Q` has loaded. Rakudo answers `v2026.07` to the same question and
sails through. The ecosystem record `ecosystem/dists/P/Proc--Q~c1053504.json`
was therefore `blocked_load` with zero files reached, against a rakudo baseline
that passes all five.

Three things make this a policy question rather than one module's quirk.

**The module's documented contract says mutsu should pass.** `RakudoPrereq`'s
README describes the `rakudo-only` option as:

> `rakudo-only` — **by default, the module would not fail if the compiler is not
> Rakudo.** Specify this option if you want to fail for non-Rakudo compilers as
> well, regardless of their version.

So the author's intent for a non-Rakudo compiler *without* `rakudo-only` is
explicitly "do not fail". The implementation only guards the *name* check behind
the flag and then falls through to a version comparison whose coordinate system
a non-Rakudo compiler has no way to answer in. `Proc::Q` uses the default, i.e.
it intends to admit non-Rakudo implementations.

**The same comparison shape recurs across the ecosystem.** It is not confined to
`RakudoPrereq`; conditional-shim loads use it directly, e.g. Net::BGP's

```raku
use Net::BGP::Conversions-Pre201812:if($*PERL.compiler.version < v2018.12);
```

(already noted in `t/modules/import-export/use-if-pragma.t`). Every instance
compares the field against a **Rakudo release date**, because for Rakudo the
compiler's release version *is* its language-level marker.

**`v0.23.0` is not a neutral non-answer.** As a `Version` it sorts below every
Rakudo release ever made, so every gate of this shape resolves to its worst
branch: minimum-version guards block the load outright, and conditional shims
select their pre-2018 code path even though mutsu implements the modern
semantics those shims exist to work around. Reporting the package version is an
active claim of extreme antiquity, not an abstention.

## Decision

**`$*RAKU.compiler.version` / `$*PERL.compiler.version` reports the Rakudo
release whose language level mutsu targets.** It is a compatibility coordinate
expressed in the ecosystem's own units, not an identifier for this build.

A single named constant carries it,
`Interpreter::RAKUDO_COMPAT_VERSION` in `src/runtime/native_methods/system.rs`,
currently `2026.07`.

**mutsu's own identity stays truthful, and mutsu never claims to be Rakudo:**

| surface | value | role |
| --- | --- | --- |
| `.name` | `mutsu` | *unchanged.* What `rakudo-only` and every `compiler.name ne 'rakudo'` check reads |
| `.auth` | `github.com/tokuhirom` | unchanged |
| `.version` | `v2026.07` | **changed.** The language-level coordinate |
| `.release` | `0.23.0` (crate version) | mutsu's own release — where "which build is this" now lives |
| `.id` | `mutsu-0.23.0` | unchanged; still the precomp-directory key |
| `.verbose-config`'s `mutsu` section | crate version, target triple | unchanged build facts |

The name is the load-bearing half of that table. Because `.name` is still
`mutsu`, a distribution that genuinely means "Rakudo only, whatever the version"
still gets its way — verified: the `rakudo-only` form of the same reduction still
exits 1 under mutsu, and only under mutsu.

### Why the compatibility claim errs upward

`v2026.07` over-claims: mutsu does not implement everything Rakudo 2026.07 does.
That is the right direction to err, for a reason specific to how the two failure
modes present.

- Over-claiming, a module reaches for a feature mutsu lacks and fails with a
  concrete "not implemented" error, naming the gap. That is an actionable entry
  on the ecosystem board and precisely the kind of finding the sweep exists to
  produce.
- Under-claiming, the module never runs at all, or silently runs a legacy code
  path written for a Rakudo that predates mutsu's actual semantics. Neither
  produces a finding, and the second can produce a *wrong answer* rather than an
  error.

The constant is therefore bumped in step with the Rakudo version mutsu is
actually measured against (`ecosystem/dists/**` `measured.raku_version`,
`TODO_roast/raku-baseline.md`) — backed by a measurement, never raised
speculatively.

## Alternatives rejected

**Leave `.version` as the crate version and treat `RakudoPrereq` as
Rakudo-specific.** Contradicted by the module's own README, which reserves that
meaning for `rakudo-only`. It also leaves the whole class of
`:if($*PERL.compiler.version < vYYYY.MM)` shims selecting legacy branches, which
is a correctness problem rather than a coverage one.

**Expose a *separate* compatibility attribute and leave `.version` alone.**
Truthful, and it reads well in isolation, but nothing in the ecosystem reads a
field mutsu invents. It would not unblock a single distribution. The field the
ecosystem actually consults is `.version`; a policy that does not reach it is
not a policy.

**Special-case `Proc::Q`, patch the extracted distribution, or bundle a
replacement `RakudoPrereq`.** All three are banned by standing project rules
(`CLAUDE.md`'s no-test-specific-hacks rule, the ecosystem loop's "never edit the
distribution" and "never special-case the distribution inside mutsu", and
BATTERIES.md rung 3 / [ADR-0096](0096-batteries-adoption-policy.md)). They also
move the failure rather than fixing it: the next distribution with the same
comparison hits it again.

**Report a Rakudo-style date from `.name` as well** (i.e. claim to *be* rakudo).
Rejected outright. It would defeat `rakudo-only`, which is the one place the
ecosystem gets to say "I really do mean Rakudo", and it is the only part of this
surface where a lie has no upside.

## Consequences

- `ecosystem/dists/P/Proc--Q~c1053504.json` moves off `blocked_load`; the
  re-measure is part of the change.
- Any future distribution gating on a Rakudo minimum is admitted, and its real
  gaps (if any) surface as ordinary failures the board can rank.
- A consumer that wants mutsu's build must read `.name` + `.release` / `.id`, or
  `.verbose-config`'s `mutsu` section. `vendor/zef`'s `FileReporter` already
  submits `.version` and `.release` as separate fields, so it keeps both.
- Pinned by `t/vm/codegen/compiler-version-rakudo-coordinate.t`, which asserts
  both halves: the version clears the two real ecosystem gate shapes, and the
  name/release/id still identify mutsu.

This supersedes the narrower rationale recorded in the code when `.version` was
first aligned to `CARGO_PKG_VERSION` (so that `.version` and `.id` would not
disagree about a build). That consistency requirement is preserved, but between
`.release` and `.id` — which is where build identity belongs.
