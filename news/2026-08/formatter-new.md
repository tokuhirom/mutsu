# `Formatter.new` now returns a reusable formatter

`Formatter.new(FORMAT_STRING)` is now available under `v6.e.PREVIEW`. It
returns a callable that reuses the parsed sprintf-style format string, so
string and numeric directives can be applied repeatedly.

