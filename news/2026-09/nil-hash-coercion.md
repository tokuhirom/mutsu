# `Nil.hash` no longer silently returns `Nil`

`Nil.hash` — `Any`'s Associative coercion, which `Nil` genuinely inherits —
answered `Nil` unchanged instead of the empty `Hash` `{}` raku gives. Found
via [Router::Right](https://raku.land/zef:OU/Router::Right)'s `url()` method,
which calls `%( Nil )` (the parser's lowering of hash-context-paren syntax to
`.hash`) as the default "no extra params" argument; binding that result to a
`Hash`-typed parameter threw `X::TypeCheck::Binding::Parameter`.

The root cause was a routing bug, not a missing implementation: the native
`.hash` builtin already had a correct `ValueView::Nil` arm
(`Value::hash(ValueMap::default())`), but `exec_call_method_op_impl`'s
`target.is_nil()` fast path — which decides, by an explicit method-name
allowlist, whether a call on a literal `Nil` invocant should "fall through to
normal dispatch" or be absorbed as a no-op returning `Nil` (mirroring raku's
`Nil.ast` etc.) — never listed `hash` in that allowlist, so every call fell
into the catch-all and was absorbed. `hash` is now listed in both copies of
the allowlist (the opcode handler and its `nil_absorbs_method` twin used by
the hyper-method path), matching raku's actual behaviour
(`Nil.hash.WHAT` is `(Hash)`).

Root-caused with `rust-gdb` breakpoints tracing the dispatch from
`exec_call_method_op` down through `try_native_method`/`try_native_method_raw`
— every one of them turned out to never even run for this call, which is what
pointed at the earlier `target.is_nil()` short-circuit instead of a bug in the
native `.hash` implementation itself.

Regression coverage: `t/types/nil-hash-coercion.t`. Two further, deeper bugs
surfaced by the same distribution's other test files — a named `token`/`rule`
declaration losing its interpolated lexicals once stored past its defining
call ([#8662](https://github.com/tokuhirom/mutsu/issues/8662)), and a closure
resolving the wrong same-named variable whenever its own captured lexical is
assigned anywhere in dead code
([#8663](https://github.com/tokuhirom/mutsu/issues/8663)) — are filed
separately as `todo:deep`; Router::Right's own ledger record stays `red`
pending those.
