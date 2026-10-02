# Three `is rw` method lvalue gaps behind CSS::Properties' `self.handling($p) = $v`

CSS::Properties assigns through a multi `is rw` method whose `Int` candidate
ends in `with self.info($p) { with .edges { self!child-handling($_) } else
{ %!handling{$p} } }`. mutsu died there with "rw method 'handling' does not
expose an assignable attribute" (#10811); three independent gaps were behind it.

- **A conditional nested in a `with` lost the routine's lvalue tail.** `with`
  lowers to `if { given }`, and the tail of that `given` body is compiled by
  `compile_when_tail_stmt`, whose `if` arm compiled the branches as plain
  values. It now routes an `is rw` routine's tail `if` through
  `compile_routine_tail_if`, as a routine's own tail `if` already was, so the
  taken branch's `%!h{$p}` comes back as its container.
- **A return type rejected a returned `Proxy`.** `--> Handling` compared the
  Proxy object itself. Like rakudo, the check now reads what the Proxy FETCHes
  and still hands the Proxy back, so the caller's write reaches its STORE.
- **A named-sub STORE never ran.** `sub STORE($, $v) { ... }; Proxy.new(:&STORE)`
  carries no compiled code of its own, and `call_proxy_callback` re-ran its AST
  body by name, which did nothing. It now goes through the ordinary sub-call
  dispatch; that by-name AST overlay path is gone.

Fixing those exposed a fourth: writing through an rw routine's container for a
*missing* key of an object hash (`my %h{Int}`) stored the entry under its
`.WHICH` string with no key object, so the key read back as the Str `"Int|1"`.
The deferred entry token now records the key object in `original_keys` when
it is made.

`t/tag-set-xhtml.t` now gets past CSS::Properties' TWEAK; the next blocker
(`&Alias::sub` through a `my constant` package alias) is #10911.
