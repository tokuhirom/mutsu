# A grammar rule's parameters are bound per call, not spliced into its source

A rule such as YAMLish's `token map(Str $indent) { <map-entry($indent)>+ % [ <.newline> $indent ] }`
used to be instantiated textually for every argument: the resolver bound the
arguments in a scratch env and spliced the values into the body's pattern
text, its code blocks and its call arguments. Every distinct indent produced a
different pattern, and because the body's call edges depended on the values,
the left-recursion proof could not go through, so every call of such a rule
took the eager growing-seed path (190 eager calls per `load-yaml` of the
60-row benchmark document).

Now a rule whose body is a regex literal and whose parameters are plain
positional scalars keeps one body for every argument list, as rakudo compiles
it (ADR-12917, direction 2). The resolver puts a parameter declaration block
in front of the body; the interpolation pre-pass and the structural parser
treat the names like an in-regex `:my` lexical, so a bare `$indent` lowers to a
match-time `VarInterp` atom. The call's binding window puts the argument
values in the env, where `VarInterp`, code blocks, `** { }` counts, call
arguments and method colon arguments read them. The left-call graph takes such
a rule's body as its node and a call with arguments to it as a static edge, so
YAMLish's rules are proven free of left recursion and run as lazy frames: the
eager calls went from 190 to 0.

`VarInterp` itself grew two fixes on the way: a `Regex` value is matched as a
pattern (as a frame, with backtracking) instead of as its source text, and the
literal match honours `:i`. A method called with colon arguments from a
parameterized rule (`<.indent-panic: $<key>, $indent, "map">`) now receives the
parameter's value instead of `Nil`.

The measured steady-state cost of one `load-yaml` did not move (about 646M
instructions per parse on both sides): the eager path's inclusive cost in the
ADR's table was the callees' own matching, which a frame does too. The same
profile located where the regex half of a parse actually goes, recorded in the
ADR's Outcome section.
