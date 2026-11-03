# `.VAR` of a Proxy stays the Proxy object when bound, passed or sunk

Slice 2 of [#15893](https://github.com/tokuhirom/mutsu/issues/15893). The
Proxy that `.VAR` returns carries a `var_object` flag (slice 1, scalar
assignment). Three more places read it as a container and FETCHed; they now
leave it alone, as Rakudo does: `my $b := $q.VAR` (`update_proxy_closure_envs`
rebuilt the Proxy unflagged), a positional argument (`f($q.VAR)`,
`auto_fetch_proxy_args*`), and a sunk `$q.VAR;` (`SinkPop`). Each answers
`Proxy` with no FETCH, matching `raku`.

Left in #15893: `%h<k> = $q.VAR` and list elements still FETCH where Rakudo keeps
the Proxy, and `$b = 5` after a bind calls STORE where Rakudo dies with "Cannot
assign to a readonly variable".
Regression test: `t/vm/writeback/proxy-var-bind-arg-sink-keep-object.t`.
