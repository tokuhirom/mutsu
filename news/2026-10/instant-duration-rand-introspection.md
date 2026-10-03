# Keep Instant and Duration rand in the method catalog

The native method catalog now records `rand` for Instant and Duration. Rakudo
exposes both methods, and mutsu's native dispatcher already recognizes them;
the catalog entries make method introspection agree with dispatch.
