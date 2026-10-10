# Proxy operands of a declined user infix, and closure captures shadowed by the caller

`RedFactory`'s `t/02-example.rakutest` stringified a `Proxy` as `"Proxy"` in `.title ~ "\n"`
(#12562). Two independent bugs were behind it:

- When a program declares a `multi infix:<~>` with `is rw` parameters (Red does), the operands
  reach the user candidates raw. If none matched, the native `~` received the unFETCHed `Proxy`
  object and stringified it. The native-fallback reduction in `call_infix_fallback` now FETCHes a
  `Proxy` operand first.
- A closure created inside a method body (registered with `^add_method`) read its captured loop
  variable through the caller chain, so a same-named lexical in the *calling* frame shadowed it
  (`Env::get_sym_frame_first`: a method body now freezes the free variables it captured into
  the upvalues of closures it creates).

Tests: `t/vm/writeback/proxy-operand-user-infix-decline.t`,
`t/routines/closure/closure-capture-vs-caller-lexical.t`.
