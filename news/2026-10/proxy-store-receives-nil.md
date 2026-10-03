# A Proxy's STORE receives an assigned Nil

Assigning `Nil` to a variable bound to a `Proxy` used to reset the value to
the container default first, so the `STORE` callback saw `Any`. The `Env`
distribution exports environment variables as such proxies and deletes the
variable when `STORE` receives `Nil` (`$USER = Nil`), so that never happened.

All three assignment paths — a local slot, a name reached through the env (an
imported or captured variable), and an assignment in expression position — now
pass the `Nil` through unchanged, and the expression form evaluates to the
Proxy's `FETCH` rather than the right-hand side. Both of `Env`'s test files pass.
