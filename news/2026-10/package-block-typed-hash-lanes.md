# A package block's typed `@`/`%` lexicals keep their declared types to themselves

The declared element and key types of a `module`/`package` block's `my @a` /
`my %h{K}` lived in name-keyed env lanes (`__mutsu_hash_key_type::%seen`) that
the block left in the enclosing env. A unit file declaring a same-named
`my %seen{Hash}` overwrote them, and the block's routines then checked their
stores against `Hash` ("expected Hash but got List"). The block now records its
lanes per package, its routines re-inject them on entry, and a lexical
declared without a type clears a caller's same-named lane. This is the
module-block twin of the class-body and `unit module` fixes (#13086, #13094).
