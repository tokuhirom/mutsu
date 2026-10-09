# Local subs in map callbacks and subset names vs same-named scalars

Two interpreter gaps found by running the SBOM::CycloneDX test suite.

A `sub` declared inside a `.map` callback was registered globally and never
unregistered, so a same-named `sub` in another callback died with
"Redeclaration of routine". The inline map/grep compile now runs such a body
as a block, whose scope restores the routine registry.

A `subset bom-ref` type object checked against `Cool`/`Str` answered False
inside a routine that had a `$bom-ref` lexical: the scalar lives in `env`
under the bare key `bom-ref`, and an undefined one holds `Any`, which
`resolve_lexical_type_key` took for an alias of the subset. An alias must now
name the same type (same leaf name).
