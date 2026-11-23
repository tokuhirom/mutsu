# The bundled OpenSSL binding is re-vendored at 0.2.10

`modules/OpenSSL/` now carries upstream
`OpenSSL:ver<0.2.10>:auth<zef:raku-community-modules>` (tag `0.2.10`, commit
`aab471d3`), copied verbatim from the fez release, up from 0.2.7. As before,
`Build.rakumod`, the Windows-only DLLs and the upstream tests stay out of the
tree, and the generated `resources/libraries.json` is kept as it was (the
library stems did not change).

The three releases in between add TLS 1.3 to the accepted `ProtocolVersion`
values; ALPN (`set-alpn-protocols`, `selected-alpn`), SNI
(`set-sni-host-name`) and negotiated-protocol introspection
(`protocol-version`, `version-code`, `version-name`) on the `OpenSSL` object,
backed by four new NativeCall bindings in `OpenSSL::SSL`; and make the
`OpenSSL::Digest` subs return a mutable `buf8` again instead of a `blob8`.
`batteries.lock` pins the new commit, so the release gate runs the 0.2.10
suite (including its extended `t/08-digest.rakutest`).
