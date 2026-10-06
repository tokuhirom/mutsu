# A `unit package` in an EVAL no longer leaks into later EVALs

`EVAL('unit package P;')` left the interpreter's current package at `P`, so the
next `EVAL('class K {}')` declared `P::K`. The EVAL string entry point now saves
the caller's package before running the snippet and restores it afterwards, so
each EVAL starts in the package of its caller (#12135).
