# `%?RESOURCES<libraries/x>` finds a library shipped under another platform's name

For an uninstalled distribution (`-I`/`use lib`), `libraries/pdf` was always mapped to
the running platform's file name (`libpdf.so`). rakudo answers the library file the
`resources/libraries` directory actually ships, so PDF::Native's `pdf.dll` resolved
there while mutsu pointed at a missing `libpdf.so`. The platform name still wins when it
exists; otherwise a `x.dll` / `x.so` / `libx.dylib` entry of that directory is used.
PDF::Native's `t/00-load.t` now matches rakudo; its other test files need the native
library and fail on rakudo too (`no_baseline`).
