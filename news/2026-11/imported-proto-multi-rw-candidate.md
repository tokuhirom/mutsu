# An imported proto with an `is rw` multi candidate no longer reuses a variable call's winner for a literal

A multi family imported through a `proto` cached its winner per argument type,
but an `is rw` candidate depends on whether the argument has a container, which
the type key does not record. After `apply-fields($var, ...)` selected the rw
candidate, `apply-fields("literal", ...)` reused it and died with "expects a
writable container". Such a family now takes the uncached walk, as the by-name
caches already do. String::Fields `t/01-basic.rakutest` passes 16/16, as on
rakudo.
