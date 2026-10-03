# A missing method on a Block or Sub dies instead of composing a Sub

`{ $_ }.abs` and `(sub ($x) { $x }).abs` used to return a
`&<composed-method:abs>` Sub. That Sub applied the method to the callable's
result. The behavior came from the last-resort fallback of
`call_method_with_values`. Raku has no such behavior for a Code value: only a
WhateverCode *expression* curries a method call (`(* - *).abs`), and mutsu's
compiler already does that through `whatever_curry`. The fallback is gone, so
the call now raises `X::Method::NotFound` like rakudo does. This applies to a
WhateverCode held in a variable too (`my $w = * - 5; $w.abs`).

The undeclared-named retry stays, so `{ $_ }.arity(:zzz)` still answers 0.
The `.?method` special case that read the composed Sub as "not found" is no
longer needed, and is removed (#11445).
