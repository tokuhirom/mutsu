# Loop labels are first-class `Label` objects

A loop label used as a term (`FOO.^name`, `f(FOO)`) used to evaluate to the
string `"FOO"`, so `next |c` with a capture holding a label died with "a Label
argument is not supported". A declared label now evaluates to a `Label` object,
built once when the parser sees the declaration, carrying `name`, `file`,
`line` and the source context its `.gist` quotes (`.Str`, `.gist` and `.raku`
render as in Rakudo).

Loop control accepts it dynamically: `next(FOO)`, `next |c`, `FOO.next` (and
the `last`/`redo` forms) raise the same labelled control signal as the static
`next FOO`, so they work from a routine called inside the labelled loop and
raise `X::ControlFlow` when no loop is there to act on. A non-`Label` argument
reports `Cannot resolve caller next(Int:D)` as Rakudo does. The `Last`/`Next`/
`Redo` opcodes and the new routine and method forms share one helper,
`loop_control_signal`. (GH #10737)
