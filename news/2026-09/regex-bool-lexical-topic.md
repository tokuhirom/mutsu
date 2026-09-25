# `Regex.Bool` matches against the regex's own `$_`

A block whose tail is a bare regex returns the `Regex` object, not a `Match`:
the match is deferred to `Regex.Bool`, and Rakudo runs it against the `$_` of
the scope the literal was written in. So `my &f = { /foo/ }; so f("foo")` is
`True` there -- the block's `$_` is bound to its argument. mutsu answered
`False`, because every boolification site (`?`/`so`, `if`/`??`, `.Bool`/`.so`)
matched against the `$_` visible where the value was boolified. That made
`List::MoreUtils`'s `after { /foo/ }, <bar baz>` return its input instead of
the empty list (#9258). `$re.Bool` on a variable (the `CallMethodMut` form)
did not even reach the regex handler and died with "No such method 'Bool'".

A regex literal in an escaping position of a callable body (a block/routine
tail, `return`, an assignment RHS, a literal element) now snapshots that
frame's `$_` onto the value: `OpCode::LoadRegexClosure` gained a `topic`
field, and the value carries it in `RegexClosure::topic` /
`RegexAdverbs::topic`. Once the body has returned its `$_` cannot change, so
the snapshot is exactly the regex's lexical topic. Mainline literals take no
snapshot, since mainline `$_` stays live and mutable. A smartmatch RHS is no
longer compiled as an escaping position, so `$x ~~ /foo/` in a routine tail
keeps loading a plain constant.

All boolification sites now share one helper, `Interpreter::regex_bool`
(`src/vm/vm_regex_bool.rs`), which prefers the captured topic and falls back
to the visible `$_`. grep/first predicates use it too when the returned regex
carries a topic, so `<bar foo baz>.grep({ /foo/ })` is `(foo)` as in Rakudo.
The map/grep body recompile and the on-the-fly routine compile mark their
compiler as a callable body so their literals capture as well.

Remaining gap: a regex created at mainline and boolified inside a routine
that assigned its own `$_` still sees the routine's topic, where Rakudo sees
the mainline one; closing it needs the regex to capture `$_`'s container
rather than a value.

Pin: `t/regex/regex-bool-lexical-topic.t`.
