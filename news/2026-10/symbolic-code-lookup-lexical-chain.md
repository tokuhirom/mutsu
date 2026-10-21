# `::('&name')` and `&::('name')` resolve through the lookup site's lexical import sets

A by-name routine lookup read the import sets of every `use`-block open at run time, so a routine
called from a `use`-block still found the block's imported routine (`sub c { ::('&fx')() }` called
inside `{ use M; c() }`). The lookup ops now record the import sets lexically around their site, as
an `EVAL` call does (#14251), and the lookup starts from those. Rakudo answers "not visible" in the
callee; so does mutsu now (#14283).
