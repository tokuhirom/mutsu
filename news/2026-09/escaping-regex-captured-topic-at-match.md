# An escaping regex matches with its own `$_`, not the subject

A regex literal is a closure: `$_` inside it is the `$_` of the scope it was
written in. `<ab cd>.map: { rx{ <$_> } }` therefore builds one regex per word.
mutsu already captured that topic when the literal escapes, but only
`Regex.Bool` read it (issue #9610). Every other form of matching set `$_` to
the subject: `~~`, `.match`, `.subst` and `.grep`. So `<$_>` interpolated the
subject itself, and each regex matched everything. `.match` even died with
"Variable '$_' is not declared". App::Lorea's `--regex` filters are built
exactly this way.

`install_regex_closure_scope` now installs the captured topic as `$_` for the
duration of the match, next to the other captured lexicals, and pins it
(`Interpreter::regex_topic_pinned`). The two sites that set `$_` to the subject
leave a pinned topic alone:

- the single-match smartmatch;
- the `<{ … }>` interpolation scratch interpreter.

A non-escaping literal is unchanged: in `"abc" ~~ / b { say $_ } /` the
embedded block still sees `abc`, as in Rakudo.
