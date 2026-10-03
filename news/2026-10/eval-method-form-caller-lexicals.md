# `$code.EVAL` reads the caller's lexicals

`my $x = 42; say Q{ $x }.EVAL` printed `(Any)` — unless the program happened
to `use` a module. A top-level `my` variable lives in a frame slot that is
mirrored into the name-keyed env only when the chunk is known to reach a
lexical by a dynamic name, and the scan that decides this recognized the
`EVAL $code` call but not the method spelling `$code.EVAL`. A `use`d module
with its own `EVAL` (Test has one) latched the process-wide flag and hid the
gap. The method form now counts as reflective too, so Grammar::Extractor's
`module $m { $compiled = $code.EVAL }` builds its grammar in a script that
loads no module. (#11399)

The anonymous grammar built that way still displays as
`Internal::abc::__ANON_GRAMMAR_0__` rather than `<anon|N>`; that is filed as
#11669.
