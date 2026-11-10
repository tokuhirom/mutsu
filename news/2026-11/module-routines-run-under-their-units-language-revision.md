# A module's routines run under the module's language revision

A sub or method from a module that declared `use v6.e.PREVIEW` / `use v6.*` was judged by the
revision of the program that called it, so `Any.snip`, `.snitch`, `Str.nomark`, `IO::Path.stem`
and `.can('snip')` were missing in the module's own code under a 6.d main (ParaSeq's
`ParaSeq.snip`). The loader now records each unit's revision, the routine stack runs a frame under
the revision of its declaring file, and the 6.e methods are visible process-wide once any 6.e unit
loaded, as in rakudo (ADR-17287).
