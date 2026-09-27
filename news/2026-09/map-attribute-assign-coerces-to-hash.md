# Assigning a Map to an untyped hash attribute now copies into a Hash

`class C { has %!h; method s { %!h = Map.new((a => 1)); %!h<c> = 2 } }` died
with "Cannot modify an immutable Map (Map)" (#9708). A plain lexical
(`my %h = Map.new(...)`) already copied the Map's pairs into a fresh, mutable
Hash on assignment, but an attribute (`%!h`/`%.h`) kept the Map's own
container identity — `%!h.WHAT` stayed `(Map)` after the store.

Every `%`/`@`-attribute assignment path (by-name `SetGlobal` and the
local-slot fast path alike) funnels its result through
`apply_attr_container_element_type`, which reads the attribute's *declared*
element type from the class registry to re-embed `Array[T]`/`Hash[T]`
metadata after the generic, name-blind coercion. For an untyped attribute
(no declared type, or one explicitly typed `Any`/`Mu`) it returned the
coerced value as-is — never clearing whatever container-level metadata
(a Map's `declared_type`, or a typed source's `value_type`/`key_type`) the
assigned value happened to carry in. The general lexical-assignment path
clears that metadata itself for an untyped `%`/`@` variable, but explicitly
skips attribute names on the assumption this function already covered them;
it did not, for the untyped case.

`apply_attr_container_element_type` now clears inherited container metadata
before returning early for an untyped attribute, matching what a lexical
assignment does. Found via the CSS::TagSet ecosystem distribution, whose
`CSS::Module` TWEAK does the equivalent of `%!prop-names = $_ ~~ Enumeration
?? .enums !! $_`.

The same call site also handles a `:=` **bind** into an attribute (the
SetGlobal opcode is shared by `=` and `:=`), and clearing unconditionally
broke that case: a bind installs the source container itself rather than
copying it, so its own type identity must survive regardless of the target
attribute's declared type. `apply_attr_container_element_type` now takes an
`is_bind` flag and skips the new clearing for a bind. Found by the
Zef::Distribution ecosystem battery's own test suite, which does
`has @!provides-specs; ...; @!provides-specs := @provides-specs` where
`@provides-specs` is `my DependencySpecification @provides-specs` — the
first version of this fix collapsed it to a bare `Array`, breaking a
`--> Array[DependencySpecification]` return check ordinarily satisfied 35/35
by `t/distribution-depends-parsing.rakutest`.

Pinned by `t/oo/attribute/hash-attribute-map-assign-coerces-to-hash.t`, which
now also covers a `:=` bind into an untyped `@`/`%` attribute keeping its
source's `Array[T]`/`Hash[T]` identity.
