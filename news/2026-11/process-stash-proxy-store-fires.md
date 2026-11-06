# A `Proxy` in the process stash fires its `STORE` on `$*X = ...`

`PROCESS::<$P> := Proxy.new(...)` followed by `$*P = 'v'` replaced the Proxy with the string: the
assignment site asked the stash whether the env's binding is the process one, and `===` on two Proxy
values fell through to `eqv`, which is never true for them, so the binding read as a lexical and the
`STORE` closure never ran (#16813). A Proxy is now identified by its callbacks, and the statement-form
assignment drains the caller-variable writeback the `STORE` records, as the other Proxy-store sites do.
