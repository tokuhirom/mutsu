# Missing nqp:: ops and a Metamodel::*HOW parser gap, from AttrX::Mooish

Working `ecosystem/dists/A/AttrX--Mooish` (`red`, 0/34 baseline files) surfaced two general gaps.

First, mutsu was missing several `nqp::` value ops AttrX::Mooish's `Attribute`/`ClassHOW`
subclasses lean on directly: `nqp::istrue`, `nqp::islist`, `nqp::hllize`, `nqp::what`,
`nqp::lock`/`nqp::unlock` (routed through the existing `Lock.lock`/`.unlock` native methods), and
the no-decontainerize siblings `nqp::istype_nd`/`nqp::isconcrete_nd`/`nqp::clone_nd` (operands are
already decontainerized once at the `call_nqp_op` boundary, so each shares its base op's
implementation). `nqp::istrue` alone was the first failure in 31 of the 34 baseline files.

Second, the parser's `when`-gobbled-block guard didn't know the `Metamodel::*HOW` family of types,
so `when Metamodel::ClassHOW { }` — exactly the pattern AttrX::Mooish's `is mooish` trait handler
uses (`given $*PACKAGE.HOW { when Metamodel::ClassHOW { ... } }`) — was misdiagnosed as an
undeclared routine call gobbling the block, even though these types already smartmatch and compose
fine at runtime. Fixed by teaching `is_known_compound_type` the bare `Metamodel::*HOW` spellings;
verified against rakudo that the `Perl6::Metamodel::*` alias is *not* pre-declared for this specific
check either, so the fix stays scoped to what rakudo itself accepts.

Together these move AttrX::Mooish from every file dying on the first `nqp::istrue` call to a real,
deeper gap: `.HOW` mints a fresh, non-identity-stable metaobject on every access, so `does`-mixing a
role into a class's own HOW never persists — filed as
[#8791](https://github.com/tokuhirom/mutsu/issues/8791), with the `$*PACKAGE`-unbound-in-`BEGIN`
prerequisite it depends on filed as
[#8790](https://github.com/tokuhirom/mutsu/issues/8790). The distribution stays `red` (0/34 parity)
until those land; 31 of 34 files now converge on the single `setup-attr-helpers` failure those
issues track, rather than the earlier `Unsupported nqp:: op: nqp::istrue` on every file.

Pinned by `t/concurrency/thread-lock/nqp-istrue-hllize-lock-ops.t` and
`t/oo/mop/when-metamodel-how-type-no-gobble.t`.
