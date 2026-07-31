# Role required methods are satisfied by name, not by signature

Fixed 2026-07-31 (Cro campaign slice 1, after the web-framework survey).
Loading the real `Cro::HTTP::BodySerializers` died with a false
`Method 'serialize' must be implemented by
Cro::HTTP::BodySerializer::WWWFormUrlEncoded because it is required by
roles: ...` — the class implements the stub as a proto/multi set typed at
`Cro::HTTP::Message`, narrower than the Cro::Core stub's `Cro::Message`
parameter.

Root cause: `resolve_class_stub_requirements` only accepted a concrete
candidate whose *positional signature exactly matched* the stub's. rakudo
satisfies a role requirement by NAME — the stub's signature is advisory
(verified: `method f()` satisfies a stub `f(Int $x, Str $y --> Str)`).

Fix (`src/runtime/registration.rs`): when no exact-signature candidate
exists, any concrete same-named method in the class — or, failing that, in
the MRO above it — satisfies the stub. The exact-signature `matching` list
still drives the multiple-candidates composition-conflict check, and a
genuinely missing implementation still fails composition.

Pin: `t/role-required-method-name-based.t` (6/6 under raku too). Effect:
`use Cro::HTTP::BodySerializers` / `Cro::HTTP::Request` / `Cro::TLS` /
`IO::Socket::Async::SSL` (against the bundled OpenSSL battery) all load;
`use Cro::HTTP::Router` now stops only at the `CBOR::Simple` nqp-ops ticket.
