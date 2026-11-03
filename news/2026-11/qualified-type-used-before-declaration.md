# `use Directory` no longer dies: a qualified type may be declared later in the file

The post-declared-type check (ADR-15760 1a) refused a *qualified* name
(`X::Directory::FileExists`) used in a method before the file declares it, so loading
the `Directory` module died with "Illegally post-declared type". rakudo resolves a
qualified name when the code runs; only a single-segment name is a compile-time
"Illegally post-declared type". The check now skips qualified names. This latent bug
was hidden by the precomp cache until the binary changed.
