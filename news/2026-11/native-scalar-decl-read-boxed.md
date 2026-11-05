# A natively declared scalar is read boxed by `~~ int`

`my int $x = 5; say $x ~~ int` printed `True`, the legacy frontend's answer. Rakudo's RakuAST
frontend reads a natively declared variable boxed, so it is `False`, while a native sub / pointy /
`is copy` parameter and an element of a destructured `my int ($p, $q)` still match `int`. The
compiler now flags a plain `my <native> $x` scalar declaration (`SmartMatchLhs::Var::boxed_native`;
an element of a destructured declaration carries the new `VarMarks::signature_element` and is not
flagged) and `direct_native_lhs_match` answers `False` for it. Tests:
`t/nativecall/native-scalar-decl-smartmatch-boxed.t`; `t/nativecall/native-value-smartmatch.t`
now pins the RakuAST answers.
