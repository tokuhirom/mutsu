# Nominal `Num` parameters are raku-strict in dispatch and binding: the Int/Rat "numeric widening" dialect is retired

Found by an ADR-0019 E9-pre follow-up probe (the redrawn-cursor model probe died mid-deferral
calling a `Num $x` candidate with an Int argument), root-caused the same day, and fixed.

**The bug.** mutsu's shared signature matcher admitted Int/Rat/FatRat arguments for a nominal
`Num` parameter ("numeric widening"), in three places: the positional and named branches of
`args_match_param_types` (`src/runtime/types/args_matching.rs`) and the binder
(`src/runtime/types/binding_signature.rs`). raku has no such widening — `Num` means floating
point, `1 !~~ Num` — so a multi call with only a `Num` candidate raised the wrong exception
(`X::TypeCheck::Binding::Parameter` from the binder after the matcher force-admitted, instead
of `X::Multi::NoMatch` from dispatch), a sub multi even bound silently and ran the body with an
Int, and a `nextsame`/`callsame` deferral walking past a `Num` candidate died in the binder
instead of skipping it.

**Where the widening came from.** Added 2026-02-28 (#554, hyper parallel dispatch) to satisfy
`roast/S12-methods/parallel-dispatch.t`'s `@a».*mul(2)` expectations — which are fudged
`#?rakudo todo` precisely because real rakudo does NOT call the `Num` candidate there (verified:
rakudo returns `[(2,), (4,), (6,)]`, only the Int candidate). The widening made mutsu "pass"
tests rakudo itself fails — the private-dialect trap. Extended to named params in #2369, whose
pin `t/multiple-signatures.t` encoded the dialect ("Num named param accepts Rat/Int value");
that pin is updated to the raku-verified behavior (rejects, `X::Multi::NoMatch`).

**The fix.** Delete all three widening branches. A 14-case ground-truth matrix (method multis,
sub multis, named params, only-methods, narrow-vs-Any siblings, `Numeric`/`Real`/`Num()`
coercion controls) now matches raku row for row: `Num` rejects Int/Rat at dispatch;
`Numeric`/`Real`/`Cool` keep matching them; `Num()` coercion keeps accepting Int; a real Num
argument still picks the `Num` candidate over `Any`. Pinned by
`t/multi-num-param-strictness.t` (11 assertions, green under both `prove -e raku` and mutsu),
including the deferral shape: `nextsame` now skips a non-matching `Num` candidate in the chain
instead of dying in the binder.

**The one legitimate consumer of the old meaning: S13 signature alternates.**
`roast/S06-signature/multiple-signatures.t` (whitelisted) relies on `Num :$n` accepting Rat —
but that file uses the `multi sub f (SigA) | (SigB) {...}` signature-ALTERNATES syntax, an
S13-era feature rakudo never implemented (rakudo cannot parse the file at all: "Missing
block"), written when `Num` still meant "any number" (today's `Numeric`). The fix confines the
old meaning to the old feature: the sub-declaration parser translates a bare `Num` constraint
to `Numeric` in every slot (primary included) of an alternates declaration, at the feature
boundary. Both roast alternates files stay green, `t/multiple-signatures.t` pins both sides of
the boundary (alternates: Rat binds; plain multi in the same file: `X::Multi::NoMatch`), and
modern dispatch stays raku-strict.

This was listed as a prerequisite/co-requisite for ADR-0019 E9a (the redrawn cursor's advance
filter must be raku-strict); it is now retired.
