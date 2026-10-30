# `*@v is raw` and `+@v is raw` bind a List

A sigiled array slurpy marked `is raw` now binds a `List` instead of an `Array`,
matching Rakudo (`sub f(+@v is raw) { @v.^name }; f(1, 2)` is `List`).
The unmarked `*@v` / `+@v` slurpies still bind an `Array`. Found while running
the `annotations` distribution (#15259); fixes #15460.
