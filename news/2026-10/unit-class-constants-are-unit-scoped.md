# `unit class` file-scope constants no longer collide across modules

A `my constant` (or sigil-less `constant`) declared in a `unit class` file was left bound
in the loading scope's environment, because the unit-scope collector only walked statements
that follow a `unit module`/`unit package` and never looked inside the `ClassDecl` body that
`unit class` wraps around the rest of the file. Two such modules each declaring
`my constant COLORS` therefore shared one binding: the first one loaded won, and the
second module's methods read the first module's data. Tag-exported constants
(`is export(:colors)`) were additionally skipped by the same collector.

Both cases now keep the constant in the module's own scope. Found through the
`Color::Names` ecosystem distribution (`t/02-find-color.rakutest` counted 6 RAL-DSP colors
instead of 143). Pinned by `t/modules/import-export/unit-class-constant-isolation.t`.
