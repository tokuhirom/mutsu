# `.WHERE` and `nqp::objectid` of a raw `BOOTHash` / `BOOTArray`

A raw VM collection (`nqp::hash`, `nqp::list`, a `Map`'s `$!storage`) answers
only the meta-level questions, as rakudo's `KnowHOW`-typed `BOOT*` objects do.
That also refused `.WHERE`, the pseudo-method every object has, so
`nqp::objectid` (which mutsu lowers to it) died with "No such method 'WHERE'
for invocant of type 'BOOTHash'". EEL 0.3's state copier keys shared capture
storage by `nqp::objectid` of a capture's raw `%!hash`, and 10 of its test files
died there (#16888). A raw collection now answers `.WHERE` with its node's
identity.
