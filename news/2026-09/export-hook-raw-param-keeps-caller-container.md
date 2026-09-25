# A raw parameter of an EXPORT-imported sub keeps the caller's container

`our sub ro(\a) is export(:S) { nqp::iscont(a) ... }` re-exported through a
custom `sub EXPORT` saw a bare value for `ro($a)`: `a.VAR.^name` said `Int`
where rakudo says `Scalar`, and Scalar::Util's `readonly($a)` answered True
(#9410). The same sub imported by a plain `is export`, or called through
`my &f = &g`, was right.

A bareword call that resolves to a lexical `&name` callable -- a `sub
EXPORT`-installed import, or a `&name` parameter shadowing a package sub --
was dispatched with `vm_call_on_value` alone, without the call site's argument
sources. The binder reads those sources to hand a raw or `is rw` parameter the
caller's container, so it bound the decontainerized value instead. Both paths
now go through `call_lexical_callable_with_sources`, which sets the sources
around the call exactly as `CallOnCodeVar` already did for code variables.

Pin: `t/modules/import-export/export-hook-raw-param-container.t`.
