# `$*ARGFILES` is an IO::ArgFiles cat handle, and `does R[...]` reblesses in place

LN 1.001002's `t/02-imports.t` now matches rakudo. LN mixes
`IO::CatHandle::AutoLines[:reset, :LN]` into `$*ARGFILES`, and the role counts lines and resets
the count on every switch to the next file. Getting there took several interpreter changes (#17754):

- `$*ARGFILES` is an `IO::ArgFiles`, a cat handle over the files in `@*ARGS`, or `$*IN` when there
  are none. `lines()`, `get()` and `.eof` read through the cat, so `on-switch` fires per file. The
  old byte-stream ArgFiles reader in the handle table is gone.
- `IO::CatHandle is IO::Handle`, as in rakudo, so `self ~~ IO::Handle` holds in AutoLines' `TWEAK`.
- `$o does R[...]` reblesses the object into `C+{R[...]}`, composed with the evaluated arguments,
  the same way a plain `$o does R` does. It used to wrap the object, and the wrapper lost what the
  role's `TWEAK` bound and wrote. The type name follows rakudo: a positional argument shows as its
  type, and named arguments are not shown (`C+{R[Int]}`, `IO::ArgFiles+{IO::CatHandle::AutoLines}`).
  `~~ R[Int]` still matches. Two objects mixed with different arguments get distinct types.
- A role's `BUILD`/`TWEAK` run by `does` sees the role's parameters bound, defaults included.
- A cat publishes its state before it calls `on-switch`, so the callback reads the new `.path`.
  `$*ARGFILES` opens its first file on first use, as rakudo does on first access.
- `run` stringifies object arguments with `.Str`, so a path with a role mixed in (Temp::Path's
  `make-temp-path`) names its file. `IO::Path.raku` shows the mixin type's name.
- A module's `END` that names a later-declared sub of that module as a bare term (Testo's
  `END { $seen-done or done-testing }`) now calls the sub. A failing Testo suite now exits non-zero.
