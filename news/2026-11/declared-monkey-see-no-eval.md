# A declared `sub MONKEY-SEE-NO-EVAL` sets the pragma, as in rakudo

In rakudo, the `MONKEY-SEE-NO-EVAL` pragma is the lexical symbol
`&MONKEY-SEE-NO-EVAL`. The regex interpolation check calls the routine visible
where the regex is compiled and goes by its answer. #18740 handled the imported
case (`use Test` exports one). A routine the program declares itself still left
the check in place: `sub MONKEY-SEE-NO-EVAL { 1 }` followed by `/<$s>/` with a
code block in `$s` died with "Prohibited regex interpolation" (#18748).

A routine declared under that name is now called once, when its scope's hoist
pass registers it, and the pragma mark is set from its answer. This is mutsu's
counterpart of rakudo's compile-time call. A variable the routine reads has not
been assigned yet, so `my $x = 0; sub MONKEY-SEE-NO-EVAL { $x }; $x = 1` still
refuses the interpolation, as on rakudo. A `my &MONKEY-SEE-NO-EVAL = sub { 1 }`
is not a declaration and does not count.

Out of scope, filed as #18937: the refusal is a plain `X::SecurityPolicy` with a
short message, where rakudo throws `X::SecurityPolicy::Eval`. That class's
`.message` is also still a stub in mutsu.
