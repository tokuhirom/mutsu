# `Attribute.auto_viv_container` is a typed container; `nqp::clone_nd` keeps a hash's type

`$attr.auto_viv_container` of an `@`/`%` attribute answered a bare Array/Hash, so
a value AttrX::Mooish built from it (`nqp::clone_nd($aviv)`, then `STORE`) lost
the attribute's element type: `has Int %.h is mooish(:lazy)` read back a plain
`Hash`. It now answers the same empty typed container an instance starts with
(`Hash[Int]`, `Array[Int]`). `nqp::clone_nd` of a typed hash copied only its
entries and dropped `Hash[T]`; it now copies the whole hash data as the array
copy already did. A definite element type (`Int:D`) is still shortened to `Int`
by the attribute metaobject (#14389). Part of #14266.
