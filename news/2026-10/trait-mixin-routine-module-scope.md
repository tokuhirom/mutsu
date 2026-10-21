# A module's trait-composed routine no longer leaks as the importer's `&name`

A routine trait that composes a role into the routine (`$r does R`, which
upstream NativeCall's `is native` does) wrote the composed routine back under
its `&name` env key. A module body runs in its loader's env, so that binding
outlived the load and became the importer's `&name`. `use Compress::Zlib` made
`.&compress` call `Compress::Zlib::Raw`'s native `compress`, which
Compress::Zlib only `need`s, instead of the `compress` it exports. That died
with a NativeCall arity error, and PDF's Flate filter could not encode a stream
(#12907).

The binding is now dropped when the load ends. The composition already lives
in the routine's identity cell (ADR-11827). By-name calls, lexical-import calls
and calls through a code value now consult that cell for a composed `CALL-ME`,
instead of the env binding. As a result, `&Pkg::f(...)` and `my &c = &Pkg::f;
c(...)` go through a trait-composed `CALL-ME` as rakudo does; before, they ran
the routine's own body.
