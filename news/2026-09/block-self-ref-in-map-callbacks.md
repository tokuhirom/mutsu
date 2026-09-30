# `&?BLOCK` inside `.map` / `.first` / `.grep` callbacks

`.map({ ... &?BLOCK ... })` ran the callback through the batched inline loop, which
skips the call machinery that installs the `&?BLOCK` self-reference, so `&?BLOCK`
was `Nil` and `.dir».&?BLOCK` died with `No such method ''`. A callback whose bytecode
reads `&?BLOCK` (`GetCodeVar`, `GetCodeVarLocal` or `CallOnCodeVar`) now takes the
per-element call path in `map`, rw-`map`, `grep` and `first`.

Found by the `TOML::Thumb` ecosystem suite (`t/valid.t`, `t/invalid.t`), which now runs.
The residue there is #10361 (`:=` rebind of an `is rw` parameter in a sub containing a
`for` loop overwrites the caller's variable). Pinned by `t/routines/block-self-ref-in-map.t`.
