# Same-named `is native` subs in sibling packages no longer clobber each other

Wrap state (`.wrap` chains and the `$!do` body upstream NativeCall binds into an
`is native` routine) is recorded under the routine's bare name. Declaring a
second routine of the same name in another package retired the first one's
chain, so `class A { sub nc(...) is native {*} }` followed by
`class B { sub nc(...) is native {*} }` made `A`'s calls run the `{*}` body and
return `*` ("expected int32 but got Whatever"). Net::BGP's TCP::LowLevel
declares `native-close` in two modules, which broke `t/31` and `t/90`.

The wrap tables now remember each wrapped routine's package: redeclaring a
routine retires only chains of its own package, a routine of a named package is
also recorded under its qualified name, and a by-name call picks the entry
whose package is the callee's.
