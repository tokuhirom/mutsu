# A subrule's bare-variable and literal arguments are read without the evaluator

A parameterized subrule call such as `<element($indent, 0)>` or
`<value=map($new-indent)>` evaluates its argument list each time the matcher
reaches it. `eval_regex_arg_list` compiled the whole list as a Capture
(`\($indent, 0)`) and ran it in a scratch evaluation env built from the
caller's env and the live captures, about 28k instructions per call. That
wrapping also hid the two fast paths a lone argument already had (a bare
`$name` read, and a literal constant), because a Capture is neither.

When every argument is a bare scalar variable or a literal constant, the list
is now read directly: a variable from the in-regex lexicals and then the
caller's env, exactly the read a lone `$name` makes, and a literal from its
cached parse. A variable's value is taken as is only when it is a plain scalar
(`Str`, `Int`, `Num`, `Rat`, `Bool`, ...), since a Capture itemizes a container
where a bare read would not. Any other list, with a named argument, a Pair, an
expression or an array, still goes through the evaluator.

Every argument in YAMLish's grammar has this shape. One warm `load-yaml` of
`bench-yaml-parse-big`'s document, `MUTSU_JIT=off`, callgrind: **120.60M ->
112.08M instructions (-7.1%)** and **77.6k -> 63.8k allocations (-17.8%)**
(#12917).
