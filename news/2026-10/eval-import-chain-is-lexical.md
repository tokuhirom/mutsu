# An EVAL starts inside its call site's import sets, not the open blocks'

An `EVAL` was compiled under the import sets of every `use`-block open when it
ran, so a routine called from such a block saw the block's imported operators
and `&` routines through its own `EVAL`. It now starts inside the sets opened in
its own frame plus the chain its code was compiled under (`eval_site_chains`).
Its name check also asks those sets, so an `EVAL` in a block can call what the
block imported by a plain `use` (it died with "Undeclared routine" before)
(#14251).
