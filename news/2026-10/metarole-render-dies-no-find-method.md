# Rendering a Rakudo metarole dies

`Metamodel::Naming.raku` / `.gist` (and a junction of metaroles) returned the
type name; on Rakudo a metarole is an NQP-level object whose `HOW` has no
`find_method`, so rendering it dies ("Cannot find method 'find_method' on
object of type NQPParametricRoleHOW"). The `Kind` module's naming relies on
this to fall back to the parameter's type name (`Kind[Junction]`), and its
`t/01-typecheck.t` and `t/02-meta.t` now pass.
