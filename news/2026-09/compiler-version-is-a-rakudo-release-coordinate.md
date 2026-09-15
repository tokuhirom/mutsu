# `$*RAKU.compiler.version` now answers in Rakudo release dates

`Proc::Q` 1.001003 could not be loaded under mutsu at all — not one of its five
test files was reached, against a rakudo baseline that passes all five. The
cause was one line of arithmetic. Its first statement is
`use RakudoPrereq v2017.05.347.g.61.ecfd.5`, and `RakudoPrereq`'s guard is

```raku
$*PERL.compiler.version before $v   # → note the message, exit 1
```

mutsu answered that question with its own crate version, `v0.23.0`, which as a
`Version` sorts below every Rakudo release ever made. The guard fired and the
process exited before `Proc::Q` had loaded.

The interesting part is that this was never one module's quirk. The field is
never compared against anything but a **Rakudo release date**, because for
Rakudo the compiler's release version *is* its language-level marker —
`RakudoPrereq` is one shape of it, and conditional shim loads such as Net::BGP's
`use Net::BGP::Conversions-Pre201812:if($*PERL.compiler.version < v2018.12)` are
another. Against every one of them, `v0.23.0` is not an abstention: it is an
active claim of extreme antiquity that resolves each gate to its worst branch.
Minimum-version guards refuse the load outright; conditional shims quietly
select a pre-2018 code path written to work around a Rakudo that predates the
semantics mutsu actually implements.

`RakudoPrereq`'s own README settles what the right answer is. It documents its
optional `rakudo-only` flag as "by default, the module would not fail if the
compiler is not Rakudo" — so admitting a non-Rakudo implementation is the
*documented default*, and `Proc::Q` uses that default. The implementation only
guards the compiler-*name* check behind the flag and then falls through to a
comparison a non-Rakudo compiler has no coordinate system to answer in.

So [ADR-0104](../../docs/adr/0104-compiler-version-is-a-rakudo-release-coordinate.md)
decides that `$*RAKU.compiler.version` / `$*PERL.compiler.version` is a
compatibility coordinate expressed in the ecosystem's units — the Rakudo release
whose language level mutsu targets, carried by one named constant
(`Interpreter::RAKUDO_COMPAT_VERSION`, currently `2026.07`, bumped only in step
with the Rakudo version mutsu is measured against).

mutsu's own identity did not move and mutsu does not claim to be Rakudo:
`.name` is still `mutsu`, `.release` is the crate version, `.id` is still
`mutsu-<crate version>`, and `.verbose-config`'s `mutsu` section still carries
the build facts. The name is the load-bearing half — because it is honest,
`RakudoPrereq`'s `rakudo-only` form of the same reduction still exits 1 under
mutsu and still succeeds under rakudo, which is exactly the discrimination that
option exists to make.

The compatibility claim deliberately errs upward. Over-claiming means a module
that reaches for something mutsu lacks fails with a concrete "not implemented"
error naming the gap — an actionable entry on the ecosystem board.
Under-claiming means the module never runs, or runs a legacy path that can
produce a wrong answer rather than an error. Only one of those two failure modes
tells us anything.

Pinned by `t/vm/codegen/compiler-version-rakudo-coordinate.t`, which asserts
both halves: the version clears the two real ecosystem gate shapes (five of its
assertions pass unchanged under rakudo, which is what makes them a spec claim
rather than a mutsu preference), and the name/release/id still identify mutsu.
