# A class-scoped sub sees its own class-body statics, not the caller's same-named array

A `sub` declared in a class body (including every exported sub of a `unit class` module file)
that read a class-body `my @a` / `my %h` resolved the name through the flattened env, so a
same-named array or hash in the calling frame won: its reads and its writes both landed on the
caller's variable. Methods already had the class statics injected on entry; the named-sub call
paths (full, fast and light) now do the same and restore the displaced bindings on exit, before
the call's env merge. Found by running `Arithmetic::PaperAndPencil` t/07-mult, whose
`infix:<☈+>` reads the module's file-scope `@digits` while its caller has an `@digits` parameter.
