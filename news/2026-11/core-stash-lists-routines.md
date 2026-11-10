# `CORE::.keys` lists the core routines

`CORE::.keys` (and `SETTING::`) answered an empty list: the pseudo-package built
a plain package stash. It now lists the core types and every core routine as
`&name`, the way the lazy `CORE::<&say>` lookup already resolved them.
Jupyter::Kernel's completion (`CORE::.keys.grep(/^prin/)`) relies on it.
