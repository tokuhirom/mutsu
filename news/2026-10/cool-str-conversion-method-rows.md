# Str and Cool conversions, Version, Mu.Numeric and Bool.pick are method-table rows

`Str.Numeric/Int/Num`, `Cool.Int/Num/Rat/UInt`, `Cool.Version`, the scalars' `Capture`, `Mu.Numeric/Int` and `Bool.pick/roll` are rows now
(ADR-11276 §9.71, #12678), sharing one routine each with the cascade arm they overlap.

Behaviour moved toward Rakudo: `"0x1F".Rat` is `31.0` and `"NaN".Rat` is `0/0`, `(1, 2, 3).UInt`, `{a => 1}.Num` and `[1, 2].Version` answer instead of
`X::Method::NotFound`, `42.Capture` names the value in its `X::Cannot::Capture`, `callsame` from a user `Numeric` reaches `Mu.Numeric` (and throws
`X::Multi::NoMatch` like Rakudo) instead of answering `Any`, and `True.pick` fails because only `Bool:U` has a candidate.
