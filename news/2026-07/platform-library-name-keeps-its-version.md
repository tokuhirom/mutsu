# `$*VM.platform-library-name` honours `:version`

`platform-library-name` mapped a library short name to the OS-specific file name
(`pq` → `libpq.so`) but dropped the `:version` adverb. A distribution ships the
ABI-versioned file and only the `-dev` package installs the bare symlink, so on
a machine with `libpq.so.5` and no `libpq.so`, a binding that probes versions
found nothing:

```raku
constant LIB = NativeLibs::Searcher.at-runtime('pq', 'PQstatus', 5);
```

`NativeLibs`' probe builds each candidate with
`$*VM.platform-library-name($libname.IO, :$version)`, so every candidate came
back unversioned, every `dlopen` missed, and `DBDish::Pg` failed to install —
three subtests of `DBIish`'s `01-basic`.

The version now lands where rakudo puts it: after the extension on Linux
(`libpq.so.5`), before it on macOS (`libpq.5.dylib`); Windows has no ABI suffix.
A `Version` type object, which is what an omitted version reaches this code as,
still yields the unversioned name.
