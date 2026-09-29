# A variable is in scope for its own initializer

A declared variable is visible in its own initializer in Raku, and mutsu
now follows that rule (issue #9770). Before this change, the initializer
still saw the outer binding: `my $x = 5; { my $x = $x + 1; say $x }` printed
`6`.

**A direct self-reference is a compile-time error.** The declaration-scope
walk in `src/parser/outer_redecl/` also checks each `my`/`our`/`state`
initializer and its trait arguments. A read of the declared name at the
declaration's own scope depth now raises
`X::Syntax::Variable::Initializer` ("Cannot use variable $x in declaration
to initialize itself"), as rakudo does. The check covers `my Int $x = $x`,
`my $x := $x`, `my $x = "$x"`, `my $f = * + $f` and `my %h is
default(%h<foo>)`. It runs for every compiled unit, so the separate
top-level-only check that `EVAL` used to run has been removed. Sigilless
declarations (`my \x = $x`) and dynamic variables are exempt.

**A nested code object sees the new binding.** The compiler now declares
the variable's local slot before it compiles the initializer, so
`my $x = do { $x }` reads the new `Any` and `my $x = sub { $x }` captures
the new variable, not the shadowed outer one. `SetVarDynamic` with
`DeclReset::Fresh` now always resets the binding before the initializer
runs. Previously the first execution of a body-local declaration left an
outer binding visible, so `my $*X = $*X + 1` in a sub read the caller's
`$*X`; it now reads the fresh `Any`, as it does in rakudo.
