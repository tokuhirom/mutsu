# `--> Mu` / `--> Any` accept a returned type object

The light call path checked a returned type object against the declared return type by name only, so
`sub next-ancestor(Mu $node, int $hops --> Mu) { $node.parent }` died with "Type check failed for
return value; expected Mu but got Selkie::Widget" whenever `parent` was an unset attribute (a type
object). `Mu` takes every type object and `Any` every one but `Mu`, as rakudo does. Selkie's
`t/111`, `t/112`, `t/113` and `t/19` now pass.
