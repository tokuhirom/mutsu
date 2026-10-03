# Fortran::Grammar passes: IO::Glob, FALLBACK actions and static capture lists

The `Fortran::Grammar` distribution's own test suite (42 tests) now passes. Its
`IO::Glob` dependency and its hash-tree action class needed seven independent
fixes:

- A regex built in a routine that interpolates a regex-valued lexical
  (`my $base = rx/a/; rx/$base$p/`) now resolves that lexical from its own
  captured scope when it is itself spliced into another regex (`rx/^$r$/`),
  instead of dying with "Variable '$base' is not declared".
- `my Pkg::Type $x` inside a class body resolves the qualified type relative to
  the enclosing package, as a signature already did.
- `my (:$a, :@b) := \(:a(5), :b([6, 7]))` binds from the Capture's named part.
- Grammar actions dispatch to an actions class's `FALLBACK` method when the
  rule has no method of its own, as Rakudo's `find_method` does.
- A Pair, Hash or bound-container value holding a not-yet-iterated `map` Seq
  is reified before `gist`/`Str`/`raku`, so `1 => $seq` no longer prints
  `1 => ()`.
- A capture name that can bind twice in one match (`<a>? ':' <a>?`) is
  list-valued from the start of the match, so it is `[]` even when neither
  occurrence bound -- Rakudo's static capture-name analysis, now applied at
  every pattern level in both the tree walk and the compiled engine.
- A quote character inside a `rule`'s character class (`<-["]>*`) no longer
  starts a quoted literal in the sigspace pass, which had silently dropped the
  rule's significant whitespace after it.
