# Every spelling of `fail` returns past the routine's own CATCH

#17880 made the `fail` statement return its Failure from a routine past that
routine's `CATCH`, as rakudo does. The other spellings — `fail` in an
expression (`my $x = fail "x"`), `$exception.fail` and the `...` stub — raise
through builtins and were still caught by the routine's `CATCH`, so the call
answered `Nil` instead of the Failure.

The builtins now decide from the code that calls them, using the same
compile-time facts the statement form uses (the calling chunk is a routine or
lies lexically in one). A `fail` in a subset's `where` block declared outside
the routine is still caught there, as on rakudo (#17913).
