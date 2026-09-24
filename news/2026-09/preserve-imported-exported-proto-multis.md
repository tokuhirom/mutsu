# Preserve imported exported-proto multi candidates

Fixed a module-loading bug where a local multi declaration could replace the
exported proto family imported from another module. The local wrapper and the
imported candidates now remain in the same dispatch family, matching Rakudo.

This restores `Markup::Calendar` parity and is pinned by
`t/modules/import-export/imported-exported-proto-local-multi.t`.
