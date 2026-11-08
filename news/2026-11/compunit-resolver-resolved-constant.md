# `$*CU.resolver`, `IMPL-BEGIN` / `IMPL-CHECK` and `Declaration::ResolvedConstant`

A CHECK-phase rewrite (Qwiratry's `BodyRewrite`) reaches for `$*CU.resolver` / `$*CU.context`
(and `$!resolver`), runs `IMPL-BEGIN` / `IMPL-CHECK` on nodes it built, and wraps compile-time
values as `Term::Declaration(Declaration::ResolvedConstant(:compile-time-value($v)))`.
All three exist now: the handles are opaque objects of rakudo's class names, the two phase
hooks accept the resolver and context and leave the node alone (the finished tree is lowered
and resolved once after the CHECK phase), and a constant term lowers to its value.
Covered by `t/rakuast/rakuast-resolved-constant.t` and `rakuast-compunit-resolver-context.t`.
