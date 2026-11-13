# `%*ENV<KEY>` FETCHes a Proxy-bound element

A literal-key `%*ENV<KEY>` read compiles to the dedicated `GetEnvIndex`
opcode, which pushed the stored element as is. When the element was bound to
a `Proxy` (`%*ENV<PATH> := Proxy.new(...)`, what the Env distribution's
`use Env <@PATH>` does), `my $copy = %*ENV<PATH>` stored the live Proxy, so
the copy changed whenever the Proxy's backing array did. The opcode now
FETCHes a Proxy element the way the general `Index` opcode does, and Env's
`t/02-specific.rakutest` passes 15/15 like rakudo.
