# A package-less module's multi families stay in its compunit

A `multi sub` or `proto sub` at the top level of a module with no package of
its own is a lexical of that compunit in Raku. Only `use` imports it, and only
when it is `is export`. mutsu registered these candidates under the shared
`GLOBAL::` keys and never scoped them. So a `need` statement,
`CompUnit::Repository.need` and an unexported multi all made the family
callable from the loading scope (#11004).

After a module load, each such family is now recorded as visible only to its
declaring compunit. An import adds the importing compunit. This is the same
per-candidate gate that imported operators already use (ADR-0131). Name probes
and `&name` respect the gate too, so a hidden family is reported as an
undeclared routine, as in Rakudo.
