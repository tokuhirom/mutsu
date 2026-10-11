# nqp::bindattr of `$!storage` / `$!reified` shares the raw store with the handle variable

`nqp::bindattr(%r, Map, '$!storage', $raw)` (and the `List` / `$!reified` form) copied `$raw`'s
contents into `%r`, so a later `nqp::bindkey($raw, ...)` or `nqp::push($raw, ...)` was lost. The
compiler now rebinds the plain variable `$raw` to the raw view of the object's own node right
after the bind, so a write through the handle is seen by the object (#12672). ADR-12666 §9 records
why the handle is redirected rather than the object, and the divergence that remains (a handle held
anywhere but that variable).
