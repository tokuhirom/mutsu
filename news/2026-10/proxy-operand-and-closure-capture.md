# A Proxy operand of a declined user infix is FETCHed

`RedFactory`'s `t/02-example.rakutest` stringified a `Proxy` as `"Proxy"` in `.title ~ "\n"`
(#12562). Red declares `multi infix:<~>` candidates with `is rw` parameters, so operands reach the
user candidates raw; when none matched, the native `~` received the unFETCHed `Proxy` object and
stringified it. The native-fallback reduction in `call_infix_fallback` now FETCHes a `Proxy`
operand first.

Test: `t/vm/writeback/proxy-operand-user-infix-decline.t`.
