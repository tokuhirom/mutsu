# The interpreter's own bookkeeping keys no longer probe for package aliases

Writing a sigilless binding stores a `__mutsu_sigilless_readonly::\name`
bookkeeping key. Its shape reads as a package-qualified name, so every write
resolved the key's head as a possible `constant` package alias (a type-name
lookup that ran the whole module-scope miss chain, about 24k instructions per
write). The AttrX::Mooish loop paid it six times per iteration. A `__mutsu_*`
key is the interpreter's own metadata and never a user package variable, so
`package_alias_var_name` now declines it up front.

The qualified-variable gate's package tables and the per-class attribute
tables are also hashed with Fx instead of SipHash: they are probed on every
method call.

Part of #15688.
