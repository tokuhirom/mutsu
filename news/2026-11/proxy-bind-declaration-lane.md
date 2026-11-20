# A Proxy bound by `my $x := EXPR` declares through the bind lane

The env-free bind-declaration lane (`news/2026-11/scalar-bind-declaration-lane.md`)
now also takes a Proxy (`my $p := substr-rw($s, 0, 1)`,
`my $attr-var := self.get_value($instance)` in AttrX::Mooish). The general
store binds a Proxy as itself: no readonly mark, no binding cell, and the
STORE closure's env updated for the name (`update_proxy_closure_envs`). The
lane now does exactly that.

Two declaration steps of the general store moved into the lane, so it no
longer has to decline when they apply:

- An earlier binding of the name may have left a cell or a Proxy in the
  visible env. The lane replaces it, so a later `$x = v` cannot reach that
  binding's STORE (#9244).
- A forward `:=` alias key or a sigilless-readonly marker of the name is
  removed when visible. Before, any earlier aliasing of a name latched
  every later `my $x := ...` of that name onto the general store.

Per bind (`my $v := self.p` with `p` returning a Proxy, callgrind slope):
33.4k → 31.3k instructions. The AttrX::Mooish `t/090` probe is unchanged
(5,275,042 → 5,274,250 Ir/round): most of its Proxy binds are of names a
closure captures, which keeps them on the general store.
