# A `my class ... is export` in a nested `module` block is exported

A lexical class declared `is export` inside a non-`unit` `module` block of a module file
was registered under a NUL-suffixed key, so the importer's short-name alias step derived a
mangled short name and the importer saw a bareword `Str` instead of the class. The alias step
now takes the source-facing name (before the NUL) and lets an `is export`-ed lexical type
through the lexical-scope filter, matching rakudo. Pinned by
`t/modules/import-export/nested-module-exported-lexical-class.t`.
