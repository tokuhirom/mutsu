# An EVAL refuses to bind a lexical of the scope around it

`my $x = 1; EVAL q[$x := 5]` rebound `$x` in mutsu. Rakudo refuses it while
compiling the EVAL with `X::Bind` ("Cannot use bind operator with this
left-hand side"), for scalars, arrays, hashes and code variables alike. The
EVAL variable scan (`eval_var_scan`) now has an outer-bind mode: a `:=` whose
target is declared by a static scope around the EVAL, and not by the snippet
itself, raises that error. A variable the snippet declares (`my @b := @a`, a
parameter, a loop variable) still binds, and so does assignment (`EVAL q[$x = 7]`).
