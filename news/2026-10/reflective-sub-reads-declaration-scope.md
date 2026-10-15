# `EVAL` and `::('$x')` in a sub see the sub's scope, not the calling block's

```raku
my $q = 'top';
sub peek { EVAL '$q' }
if True { my $q = 'block'; say peek() }   # top (was: block)
```

A named sub at program scope that looks a lexical up by string now reads the
binding visible where the sub is declared. Before, it read the same-named `my`
of whatever block called it. Writes through `::('$q') = ...` and `EVAL` reach
the right variable as well. The sub's own `my` and parameters still win.

Such a sub now captures every lexical visible at its declaration, not only its
free variables, the way ADR-0024 captures free variables. `::('$x')` reads and
writes go through the same lookup as a plain `$x` (ADR-12529 phase 3,
#13604). Two leaks remain open on #13604: names declared only inside a calling
block, and variables imported by a block-scoped `use`.
