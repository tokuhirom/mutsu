# Proxy `.WHAT` fetches the proxied value

`Proxy` reads now run `FETCH` before `.WHAT` dispatch, so type introspection reports the
fetched value instead of the `Proxy` container. Both ordinary VM dispatch and the qualified
method path apply the rule; `t/proxy-what-fetch.t` pins the returned type and single FETCH.
