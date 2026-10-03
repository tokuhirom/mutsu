# `src/vm/` builds and classifies qualified names once per symbol

Every run-time package-name string surgery site under `src/vm/` is gone:
98 `format!("{pkg}::{name}")` joins, `== "GLOBAL"` compares and
`contains`/`rsplit_once("::")` scans. Each one now goes through the memoizing
constructors in `src/qualified.rs` and uses the `Symbol` its caller already
holds. A routine's `LEAVE` key, a bareword's `Pkg::tail` split, the
`GLOBAL`-package gates on the call paths and the type-declaration
qualification are all decided once per symbol, instead of on every execution.

`src/qualified/split.rs` adds the read-side helpers these sites needed:
`split_qualified`, `last_segment`, `is_inside_package`, `segments` and
`is_type_capture`. The `src/vm/` rows of `scripts/name-scans-baseline.txt` are
deleted, so `make check-name-scans` now holds the directory at zero. This is
one slice of #11507.
